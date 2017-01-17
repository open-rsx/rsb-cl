;;;; in-connector.lisp --- In-direction connector for socket transport.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

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
