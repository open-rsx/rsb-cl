;;;; bus.lisp --- Superclass for bus provider classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass bus (broadcast-processor
               print-items:print-items-mixin)
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
        (log:info "~@<~A~:@_~
                   added   connections ~{~A~^, ~}~:@_~
                   removed connections ~{~A~^, ~}~@:>"
                  bus added removed)

        ;; Install our handler and error policy in added connections.
        (iter (for connection in added)
              ;; Add ourselves to the handlers of the added connection.
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

(defmethod notify ((recipient rsb.transport:connector)
                   (subject   bus)
                   (action    (eql :attached)))
  (log:debug "~@<Connector ~A is attaching to bus provider ~A~@:>"
            recipient subject)
  (with-locked-bus (subject)
    (push recipient (bus-connectors subject))))

(defmethod notify ((recipient rsb.transport:connector)
                   (subject   bus)
                   (action    (eql :detached)))
  (log:debug "~@<Connector ~A is detaching from bus provider ~A~@:>"
            recipient subject)
  (with-locked-bus (subject)
    (removef (bus-connectors subject) recipient))
  (close-removed-connections subject))

(defmethod notify ((recipient bus)
                   (subject   (eql t))
                   (action    (eql :detached)))
  ;; Schedule remaining connections for removal when all connectors
  ;; detach.
  (setf (bus-connections recipient) '()))

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

(defmethod dispatch ((processor bus) (event notification))
  ;; Dispatch incoming or outgoing notification EVENT to interested
  ;; connectors.
  (let+ ((scope (make-scope
                 (bytes->string (notification-scope event))))
         ((&flet do-scope (scope connectors)
            (declare (ignore scope))
            (handle connectors event))))
    (declare (dynamic-extent #'do-scope))
    (rsb.ep:scope-trie-map #'do-scope scope (bus-%in-connectors processor))))

(defmethod handle ((sink bus) (data notification))
  ;; Send outgoing notification DATA to remote peer(s).
  (mapc (rcurry #'handle data)
        (with-locked-bus (sink :connections? t)
          (copy-list (bus-connections sink))))

  ;; Dispatch to our own connectors.
  (dispatch sink data))

(defmethod handle ((sink bus) (data cons))
  ;; Handle incoming notification DATA.
  (let+ (((received-via . notification) data))
    ;; Dispatch to all connections except the one from which we
    ;; received the notification.
    (iter (for connection in (with-locked-bus (sink :connections? t)
                               (copy-list (bus-connections sink))))
          (unless (eq received-via connection)
            ;; We can ignore errors here since we installed an error
            ;; policy in CONNECTION that removes CONNECTION.
            (ignore-errors
             (handle connection notification))))

    ;; Dispatch to our own connectors.
    (dispatch sink notification)))

;;;

(defmethod print-items:print-items append ((object bus))
  (let+ (((&structure-r/o bus- connections connectors) object))
    `((:connection-count ,(length connections) "(S ~D)")
      (:connector-count  ,(length connectors)  " (C ~D)"
       ((:after :connection-count))))))
