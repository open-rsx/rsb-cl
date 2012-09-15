;;;; in-connector.lisp --- In-direction connector for socket transport.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass in-connector (connector
                        timestamping-receiver-mixin
                        restart-message-receiver-mixin
                        broadcast-processor
                        expose-transport-metrics-mixin)
  ((scope :type     scope
          :accessor connector-scope
          :documentation
          "Stores the scope to which the connector is attached."))
  (:metaclass connector-class)
  (:documentation
   "Superclass for in-direction socket connectors. Instances of this
class observe a bus (which owns the actual socket) when attached and
queue received events for delivery."))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (setf (connector-scope connector) scope)
  (call-next-method)
  (with-locked-bus ((connector-bus connector))
    (push connector (handlers (connector-bus connector)))))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (with-locked-bus ((connector-bus connector))
    (removef (handlers (connector-bus connector)) connector))
  (call-next-method))

(defmethod message->event ((connector    in-connector)
                           (notification notification)
                           (wire-schema  t))
  (let+ (((&structure-r/o connector- converter) connector))

    ;; If message could be unpacked into a `notification' instance,
    ;; try to convert it, and especially its payload, into an `event'
    ;; instance and an event payload. There are three possible
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
          :encoded          (list notification) ; TODO(jmoringe): hack
          :format-control   "~@<After unpacking, the ~
                             notification~_~A~_could not be converted ~
                             into an event.~:@>"
          :format-arguments (list (with-output-to-string (stream)
                                    (describe notification stream)))))
      (notification->event connector notification))))
