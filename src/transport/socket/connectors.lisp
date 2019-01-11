;;;; connectors.lisp --- Superclasses for socket-based connectors.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; `connector'

(defclass connector (rsb.transport:connector
                     conversion-mixin)
  ((bus                     :accessor connector-bus
                            :documentation
                            "Stores the bus object representing the
                             socked-based bus to which the connector
                             provides access.")
   ;; Option slots
   (server?                 :type     (or boolean (eql :auto))
                            :reader   connector-server?
                            :initform :auto
                            :documentation
                            "Controls whether the connector takes the
                             server or client role for the bus.")
   (if-leftover-connections :type     leftover-connections-policy
                            :reader   connector-if-leftover-connections
                            :initform :wait
                            :documentation
                            "Controls whether a serer created for this
                             connector delays its shutdown until for
                             all client connections are closed."))
  (:metaclass connector-class)
  (:options
   (:server                  &slot server?)
   (:if-leftover-connections &slot if-leftover-connections))
  (:documentation
   "Superclass for socked-based connector classes."))

;; TODO(jmoringe, 2011-12-14): temp solution until config system works properly
(defmethod initialize-instance :after
    ((instance connector)
     &key
     server?
     server
     (if-leftover-connections nil if-leftover-connections-supplied?))
  (setf (slot-value instance 'server?)
        (let ((value (or server? server)))
          (etypecase value
            ((member t nil :auto)
             value)
            (string
             (cond
               ((string= value "0")    nil)
               ((string= value "1")    t)
               ((string= value "auto") :auto))))))
  (when if-leftover-connections-supplied?
    (setf (slot-value instance 'if-leftover-connections)
          (let ((value if-leftover-connections))
            (etypecase value
              (leftover-connections-policy
               value)
              (string
               (switch (value :test #'string=)
                 ("close" :close)
                 ("wait"  :wait)
                 (t       (error "~@<Invalid value ~S.~@:>" value)))))))))

(defmethod notify ((connector connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (let+ (((&structure connector-
                      transport server? if-leftover-connections bus)
          connector)
         (role    (case server?
                    ((t)   :server)
                    ((nil) :client)
                    (t     server?)))
         (address (make-connection-address transport connector))
         (options (make-connection-options transport connector)))
    ;; Depending on whether connecting to the socket-based bus as a
    ;; client or server has been requested, request a suitable bus
    ;; access provider.
    (setf bus (apply #'transport-ensure-bus
                     transport role connector address
                     :if-leftover-connections if-leftover-connections
                     options))

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
                        error-policy-handler-mixin
                        restart-handler-mixin
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
  (:direction :in)
  (:documentation
   "Superclass for in-direction socket connectors.

    Instances of this class observe a bus (which owns the actual
    socket) when attached and deliver received events."))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (setf (connector-scope connector) scope)
  (call-next-method))

(defmethod notify :after ((connector in-connector)
                          (bus       bus)
                          (action    (eql :attached)))
  (notify bus connector (rsb.ep:subscribed (connector-scope connector))))

(defmethod notify :after ((connector in-connector)
                          (bus       bus)
                          (action    (eql :detached)))
  (notify bus connector (rsb.ep:unsubscribed (connector-scope connector))))

(defmethod handle ((connector in-connector)
                   (data      notification))
  ;; TODO(jmoringe): condition translation?
  (when-let ((event (notification->event connector data :undetermined)))
    (dispatch connector event)))

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

;;; `out-connector'

(defclass out-connector (error-handling-sender-mixin
                         restart-notification-sender-mixin
                         timestamping-sender-mixin
                         connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Superclass for out-direction socket connectors."))

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
