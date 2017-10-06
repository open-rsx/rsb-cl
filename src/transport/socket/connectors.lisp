;;;; connectors.lisp --- Superclasses for socket-based connectors.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; `connector'

(defclass connector (rsb.transport:connector
                     conversion-mixin)
  ((bus      :accessor connector-bus
             :documentation
             "Stores the bus object representing the socked-based bus
              to which the connector provides access.")
   ;; Option slots
   (host     :initarg  :host
             :type     string
             :reader   connector-host
             :initform *default-host*
             :documentation
             "The name of the host on which the server is listener in case of clients and the bind address in case of the server.")
   (port     :type     (unsigned-byte 16)
             :reader   connector-port
             :initform *default-port*
             :documentation
             "The port on which the server is listening in case of clients and the port on which connections should be accepted in case of the server.")
   (portfile :initarg  :portfile
             :type     (or null string)
             :reader   connector-portfile
             :initform nil
             :documentation
             "Optionally stores the name of a file (or \"-\" for standard-output) into which an automatically assigned port number should be written when acting as server on an arbitrary free port (indicated by port number 0).")
   (server?  :type     (or boolean (eql :auto))
             :reader   connector-server?
             :initform :auto
             :documentation
             "Controls whether the connector takes the server or client role for the bus.")
   (nodelay? :type     boolean
             :reader   connector-nodelay?
             :initform t
             :documentation
             "Controls whether decreased throughput should be traded for reduced latency by the connector. For TCP connections this means the TCP_NODELAY option should be set on the socket implementing the bus connection."))
  (:metaclass connector-class)
  (:transport :socket)
  (:default-initargs
      :host (missing-required-initarg 'connector :host)
    :port (missing-required-initarg 'connector :port))
  (:options
   (:host       &slot)
   (:port       &slot port)
   (:portfile   &slot)
   (:server     &slot server?)
   (:tcpnodelay &slot nodelay?))
  (:documentation
   "Superclass for socked-based connector classes."))

;; TODO(jmoringe, 2011-12-14): temp solution until config system works properly
(defmethod initialize-instance :after ((instance connector)
                                       &key
                                       port
                                       portfile
                                       server?
                                       server
                                       tcpnodelay
                                       nodelay?)
  (let ((port     (etypecase port
                    (string
                     (parse-integer port))
                    ((unsigned-byte 16)
                     port)))
        (server?  (let ((value (or server? server)))
                    (etypecase value
                      ((member t nil :auto)
                       value)
                      (string
                       (cond
                         ((string= value "0")    nil)
                         ((string= value "1")    t)
                         ((string= value "auto") :auto))))))
        (nodelay? (let ((value (or tcpnodelay nodelay?)))
                    (etypecase value
                      (boolean value)
                      (string
                       (cond
                         ((string= value "0") nil)
                         ((string= value "1") t)
                         (t                   (error "~@<Invalid value ~S.~@:>"
                                                     value))))))))
    (when (and (not (eq server? t)) portfile)
      (incompatible-initargs 'connector
                             :portfile portfile
                             :server?  server?))
    (setf (slot-value instance 'port)     port
          (slot-value instance 'server?)  server?
          (slot-value instance 'nodelay?) nodelay?)))

(defmethod notify ((connector connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (let+ (((&structure connector- transport host port server? bus) connector)
         (role (case server?
                 ((t)   :server)
                 ((nil) :client)
                 (t     server?))))
    ;; Depending on whether connecting to the socket-based bus as a
    ;; client or server has been requested, request a suitable bus
    ;; access provider.
    (setf bus (transport-ensure-bus transport host port role connector))

    ;; Notify the bus access provider of the added connector.
    ;; ensure-bus-* already attached CONNECTOR.
    ;; (notify connector bus :attached)
    (when (next-method-p)
      (call-next-method))))

(defmethod notify ((connector connector)
                   (scope     scope)
                   (action    (eql :detached)))
  ;; Notify the bus access provider of the removed connector.
  (notify connector (connector-bus connector) :detached)

  (when (next-method-p)
    (call-next-method)))

;;; `in-connector'

(defclass in-connector (connector
                        restart-notification-receiver-mixin
                        restart-dispatcher-mixin
                        timestamping-receiver-mixin
                        broadcast-processor
                        expose-transport-metrics-mixin)
  ((scope :type     scope
          :accessor connector-scope
          :documentation
          "Stores the scope to which the connector is attached."))
  (:metaclass connector-class)
  (:documentation
   "Superclass for in-direction socket connectors.

    Instances of this class observe a bus (which owns the actual
    socket) when attached and queue received events for delivery."))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (setf (connector-scope connector) scope)
  (call-next-method))

(defmethod notify :after ((connector in-connector)
                          (bus       bus)
                          (action    (eql :attached)))
  (rsb.ep:sink-scope-trie-add
   (bus-%in-connectors bus) (connector-scope connector) connector)
  (log:debug "~@<Scope trie of ~A after adding ~
              ~A:~@:_~/rsb.ep::print-trie/~@:>"
             bus connector (bus-%in-connectors bus)))

(defmethod notify :after ((connector in-connector)
                          (bus       bus)
                          (action    (eql :detached)))
  (rsb.ep:sink-scope-trie-remove
   (bus-%in-connectors bus) (connector-scope connector) connector)
  (log:debug "~@<Scope trie of ~A after removing ~
              ~A:~@:_~/rsb.ep::print-trie/~@:>"
             bus connector (bus-%in-connectors bus)))

(defmethod notification->event ((connector    in-connector)
                                (notification notification)
                                (wire-schema  t))
  (let+ (((&accessors-r/o (converter connector-converter)) connector)
         (expose-wire-schema?  (connector-expose?
                                connector :rsb.transport.wire-schema))
         (expose-payload-size? (connector-expose?
                                connector :rsb.transport.payload-size)))
    ;; NOTIFICATION has been unpacked into a `notification' instance,
    ;; now try to convert it, and especially its payload, into an
    ;; `event' instance and an event payload. There are three possible
    ;; outcomes:
    ;; 1. The notification (maybe in conjunction with previously
    ;;    received notifications) forms a complete event
    ;;    a) The payload conversion succeeds
    ;;       In this case, an `event' instance is returned
    ;;    b) The payload conversion fails
    ;;       In this case, an error is signaled
    ;; 2. The notification does not form a complete event
    ;;    In this case, nil is returned.
    (with-condition-translation
        (((error decoding-error)
          :encoded          notification
          :format-control   "~@<After unpacking, the ~
                             notification~_~A~_could not be converted ~
                             into an event.~:@>"
          :format-arguments (list (with-output-to-string (stream)
                                    (describe notification stream)))))
      (notification->event*
       converter notification
       :expose-wire-schema?  expose-wire-schema?
       :expose-payload-size? expose-payload-size?))))

;;; `in-pull-connector'

(defclass in-pull-connector (error-handling-pull-receiver-mixin
                             in-connector)
  ((queue :type     lparallel.queue:queue
          :reader   connector-queue
          :initform (lparallel.queue:make-queue)
          :documentation
          "Stores notifications as they arrive via the message bus."))
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "In-direction, push-style communication over a socket."))

(register-connector :socket :in-pull 'in-pull-connector)

(defmethod handle ((connector in-pull-connector)
                   (data      notification))
  ;; Put DATA into the queue of CONNECTOR for later retrieval.
  (lparallel.queue:push-queue data (connector-queue connector)))

(defmethod receive-notification ((connector in-pull-connector)
                                 (block?    (eql nil)))
  ;; Extract and return one event from the queue maintained by
  ;; CONNECTOR, if there are any. If there are no queued events,
  ;; return nil.
  (lparallel.queue:try-pop-queue (connector-queue connector)))

(defmethod receive-notification ((connector in-pull-connector)
                                 (block?    t))
  ;; Extract and return one event from the queue maintained by
  ;; CONNECTOR, if there are any. If there are no queued events,
  ;; block.
  (lparallel.queue:pop-queue (connector-queue connector)))

(defmethod emit ((connector in-pull-connector) (block? t))
  ;; Maybe block until a notification is received. Try to convert into
  ;; an event and return the event in case of success. In blocking
  ;; mode, wait for the next notification.
  (iter (let* ((payload (receive-notification connector block?))
               (event   (when payload
                          (notification->event
                           connector payload :undetermined))))

          ;; Due to non-blocking receive mode and error handling
          ;; policies, we may not obtain an `event' instance from the
          ;; notification.
          (when event
            (dispatch connector event))
          (when (or event (not block?))
            (return event)))))

(defmethod print-items:print-items append ((object in-pull-connector))
  (let ((count (lparallel.queue:queue-count (connector-queue object))))
    `((:queue-count ,count " (~D)" ((:after :url))))))

;;; `in-push-connector'

(defclass in-push-connector (error-policy-handler-mixin
                             restart-handler-mixin
                             in-connector)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "In-direction, push-style communication over a socket."))

(register-connector :socket :in-push 'in-push-connector)

(defmethod handle ((connector in-push-connector)
                   (data      notification))
  ;; TODO(jmoringe): condition translation?
  (when-let ((event (notification->event connector data :undetermined)))
    (dispatch connector event)))

;;; `out-connector'

(defclass out-connector (error-handling-sender-mixin
                         restart-notification-sender-mixin
                         timestamping-sender-mixin
                         connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Out-direction communication over a socket."))

(register-connector :socket :out 'out-connector)

(defmethod handle ((connector out-connector)
                   (event     event))
  (send-notification connector (event->notification connector event)))

(defmethod event->notification ((connector out-connector)
                                (event     event))
  ;; Delegate conversion to `event->notifications'. The primary
  ;; purpose of this method is performing the conversion with restarts
  ;; installed.
  (event->notification* connector event))

(defmethod send-notification ((connector    out-connector)
                              (notification notification))
  (handle (connector-bus connector) notification))
