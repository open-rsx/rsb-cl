;;;; in-connector.lisp --- Superclass for in-direction connector classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `in-connector' class

(defclass in-connector (connector
                        timestamping-receiver-mixin
                        restart-notification-receiver-mixin
                        broadcast-processor
                        assembly-mixin
                        expose-transport-metrics-mixin)
  ()
  (:metaclass connector-class)
  (:documentation
   "This class is intended to be used as a superclass of in-direction
connector classes for Spread."))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (ref-group (connector-connection connector) (scope->group scope)))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (when (zerop (nth-value 1 (unref-group (connector-connection connector)
                                         (scope->group scope))))
    (notify connector t :detached)))

(defmethod receive-notification ((connector in-connector)
                                 (block?    t))
  ;; Delegate receiving a notification to the connection of CONNECTOR.
  (let+ (((&values buffer length)
          (receive-message (connector-connection connector) block?)))
    (values (cons buffer length) :undetermined)))

(defmethod notification->event ((connector    in-connector)
                                (notification cons)
                                (wire-schema  t))
  (let+ (((&structure-r/o connector- assembly-pool converter) connector)
         ((buffer . length)    notification)
         (expose-wire-schema?  (connector-expose? connector :rsb.transport.wire-schema))
         (expose-payload-size? (connector-expose? connector :rsb.transport.payload-size))
         notification)

    ;; Try to unpack NOTIFICATION into a `notification'
    ;; instance. Signal `decoding-error' if that fails.
    (handler-bind
        ((error (lambda (condition)
                  (error 'decoding-error
                         :encoded          buffer
                         :format-control   "~@<The data could not be ~
                                            unpacked as a protocol ~
                                            buffer of kind ~S.~:@>"
                         :format-arguments '(fragmented-notification)
                         :cause            condition))))
      (setf notification (pb:unpack
                          buffer (make-instance 'fragmented-notification)
                          0 length)))

    ;; If NOTIFICATION could be unpacked into a `notification' instance,
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
    (handler-bind
        ((error (lambda (condition)
                  (error 'decoding-error
                         :encoded          notification
                         :format-control   "~@<After unpacking, the ~
                                            notification~_~A~_could ~
                                            not be converted into an ~
                                            event.~:@>"
                         :format-arguments `(,(with-output-to-string (stream)
                                                (describe notification stream)))
                         :cause            condition))))
      (notification->event* assembly-pool converter notification
                            :expose-wire-schema?  expose-wire-schema?
                            :expose-payload-size? expose-payload-size?))))
