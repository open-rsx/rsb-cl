;;;; bus.lisp --- Superclass for bus provider classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defmacro with-locked-bus ((bus
                            &rest args
                            &key
                            connections?
                            connectors?)
                           &body body)
  "Execute BODY with BUS' lock held."
  (check-type connections? boolean)
  (check-type connectors?  boolean)

  (flet ((maybe-with-lock (which requested? body)
           (if requested?
               `((bt:with-recursive-lock-held ((,which ,bus))
                   ,@body))
               body)))
    `(progn
       ,@(maybe-with-lock
          'bus-connections-lock (or (not args) connections?)
          (maybe-with-lock 'bus-connectors-lock (or (not args) connectors?)
           body)))))

(defclass bus (broadcast-processor)
  ((connections         :type     list
                        :accessor bus-connections
                        :initform '()
                        :documentation
                        "Stores a list of connections to other
                         processes using the bus.")
   (connections-lock    :reader   bus-connections-lock
                        :initform (bt:make-recursive-lock "Bus Connections Lock")
                        :documentation
                        "Stores a lock that can be used to protect the
                         connection list of the bus from concurrent
                         modification.")
   (connectors          :type     list
                        :accessor bus-connectors
                        :initform '()
                        :documentation
                        "Stores a list of local connectors connected
                         to the bus.")
   (connectors-lock     :reader   bus-connectors-lock
                        :initform (bt:make-recursive-lock "Bus Connectors Lock")
                        :documentation
                        "Stores a lock that can be used to protect the
                         connector list of the bus from concurrent
                         modification.")
   (in-connectors       :type     rsb.ep:sink-scope-trie
                        :reader   bus-%in-connectors
                        :initform (rsb.ep:make-sink-scope-trie)
                        :documentation
                        "Stores a trie of local in-direction
                         connectors connected to the bus.")
   (options             :initarg  :options
                        :type     list
                        :accessor bus-options
                        :initform '()
                        :documentation
                        "Stores a plist of connection options which
                         should be used by connections associated to
                         the bus instance.")
   (proxy               :type     function
                        :accessor bus-%proxy
                        :documentation
                        "Stores a functions that is used as a handler
                         for `bus-connection' instances.")
   (removed-connections :type     lparallel.queue:queue
                        :reader   bus-%removed-connections
                        :initform (lparallel.queue:make-queue)
                        :documentation
                        "Stores a list of connections queued for
                         closing after having been removed from the
                         bus."))
  (:default-initargs
   :host    (missing-required-initarg 'bus :host)
   :port    (missing-required-initarg 'bus :port)
   :options (missing-required-initarg 'bus :options))
  (:documentation
   "This class is intended to be used as a superclass of client and
server providers of bus access. It manages a list of connections to
remote processes using the bus and a list of local connectors
connected to the bus."))

(defmethod shared-initialize :after ((instance   bus)
                                     (slot-names t)
                                     &key)
  (setf (bus-%proxy instance)
        (lambda (connection data)
          (handle instance (cons connection data)))))

;;; State management

(defmethod (setf bus-connections) :around ((new-value   list)
                                           (bus         bus))
  (let+ (((&structure-r/o bus- (old-value connections) (proxy %proxy)) bus))
    (declare (type function proxy))
    (prog1
        (call-next-method)
      (let ((added   (set-difference new-value old-value))
            (removed (set-difference old-value new-value)))
        (log:info "~@<~A~:@_added   connections ~{~A~^, ~}~:@_removed ~
                   connections ~{~A~^, ~}~@:>"
                  bus added removed)

        ;; Install our handler and error policy in added connections.
        (iter (for connection in added)
              ;; Add ourselves as handlers to the added connection.
              (push (curry proxy connection) (handlers connection))

              ;; Install an error policy that removes the connection.
              (log:info  "~@<~A is installing error policy for connection ~A~@:>"
                         bus connection)
              (setf (processor-error-policy connection)
                    (lambda (condition)
                      (declare (ignore condition))
                      (log:info "~@<~A is removing connection ~A after error policy~@:>"
                                 bus connection)
                      (with-locked-bus (bus :connections? t)
                        (removef (bus-connections bus) connection))
                      (close-removed-connections bus)))

              ;; Start the connection.
              (log:info "~@<~A is starting connection ~A~@:>"
                        bus connection)
              (start-receiver connection))

        ;; Close removed connections.
        (iter (for connection in removed)
              ;; Prevent the error handling from being executed
              ;; concurrently/recursively.
              (log:info "~@<~A may be closing connection ~A after remove~@:>"
                        bus connection)
              (setf (handlers connection) '())
              (lparallel.queue:push-queue
               connection (bus-%removed-connections bus)))))))

(defmethod (setf bus-connectors) :around ((new-value list)
                                          (bus       bus))
  (let ((old-value (bus-connectors bus)))
    (prog1
        (call-next-method)
      (cond
        ((and old-value (not new-value))
         (log:info "~@<~A has no more connectors~@:>" bus)
         (notify bus t :detached))
        ((and (not old-value) new-value)
         (log:info "~@<~A got its first connector~@:>" bus)
         (notify bus t :attached))))))

(defmethod notify ((connector rsb.transport:connector)
                   (bus       bus)
                   (action    (eql :attached)))
  (log:debug "~@<~A is attaching connector ~A to bus provider ~A~@:>"
            bus connector bus)
  (with-locked-bus (bus)
    (push connector (bus-connectors bus))))

(defmethod notify ((connector rsb.transport:connector)
                   (bus       bus)
                   (action    (eql :detached)))
  (log:debug "~@<~A is detaching connector ~A from bus provider ~A~@:>"
            bus connector bus)
  (with-locked-bus (bus)
    (removef (bus-connectors bus) connector))
  (close-removed-connections bus))

(defmethod notify ((bus     bus)
                   (subject (eql t))
                   (action  (eql :detached)))
  ;; Schedule remaining connections for removal when all connectors
  ;; detach.
  (setf (bus-connections bus) '()))

(defun close-removed-connections (bus)
  (iter (for removed next (lparallel.queue:try-pop-queue
                           (bus-%removed-connections bus)))
        (while removed)
        (log:info "~@<~A is closing removed connection ~A~@:>" bus removed)
        (handler-case (disconnect removed :handshake :send)
          (error (condition)
            (log:warn "~@<~A encountered error closing ~
                       connection ~A after remove: ~A~@:>"
                      bus removed condition)))
        (log:info "~@<~A closed removed connection ~A~@:>" bus removed)))

;;; Sending and receiving

(defmethod dispatch ((bus  bus)
                     (data notification))
  "Dispatch DATA to interested connectors."
  (let+ ((scope (make-scope
                 (bytes->string (notification-scope data))))
         ((&flet do-scope (connectors)
            (handle connectors data))))
    (declare (dynamic-extent #'do-scope))
    (rsb.ep:scope-trie-map #'do-scope scope (bus-%in-connectors bus))))

(defmethod handle ((bus          bus)
                   (notification notification))
  "This method is used for outgoing notifications."
  ;; Send to remote peer(s).
  (mapc (rcurry #'handle notification)
        (with-locked-bus (bus :connections? t)
          (copy-list (bus-connections bus))))

  ;; Dispatch to our own connectors.
  (dispatch bus notification))

(defmethod handle ((bus  bus)
                   (data cons))
  "This method is used for incoming notifications."
  (let+ (((received-via . notification) data))
    ;; Dispatched to all connections except the one from which we
    ;; received the notification.
    (iter (for connection in (with-locked-bus (bus :connections? t)
                               (copy-list (bus-connections bus))))
          (unless (eq received-via connection)
            ;; We can ignore errors here since we installed an error
            ;; policy in CONNECTION that removes CONNECTION.
            (ignore-errors
             (handle connection notification))))

    ;; Dispatch to our own connectors.
    (dispatch bus notification)))

;;;

(defmethod print-object ((object bus) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(S ~D) (C ~D)"
            (length (bus-connections object))
            (length (bus-connectors  object)))))
