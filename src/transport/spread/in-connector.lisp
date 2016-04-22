;;;; in-connector.lisp --- Superclass for in-direction connector classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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
  (let+ (((&values ref-count group-count promise)
          (ref-group (connector-connection connector) (scope->group scope)
                     :waitable? t)))
    ;; When this was the initial reference to the initial group of the
    ;; connection, attach CONNECTOR.
    (when (and (= ref-count 1) (= group-count 1))
      (notify connector t :attached))
    ;; If necessary, wait for the Spread group joining operation to
    ;; complete.
    (lparallel:force promise)))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (let+ (((&values &ign group-count promise)
          (unref-group (connector-connection connector) (scope->group scope)
                       :waitable? t)))
    ;; If necessary, wait for the Spread group leaving operation to
    ;; complete.
    (lparallel:force promise)
    ;; If this was the final reference to the final group of the
    ;; connection, detach CONNECTOR.
    (when (zerop group-count)
      (notify connector t :detached))))

(defmethod notify ((connector in-connector)
                   (scope     (eql t))
                   (action    (eql :detached)))
  (call-next-method)
  (detach (connector-assembly-pool connector)))

(defmethod receive-notification ((connector in-connector)
                                 (block?    t))
  ;; Delegate receiving a notification to the connection of CONNECTOR.
  (values (receive-message (connector-connection connector) block?)
          :undetermined))

(defmethod notification->event ((connector    in-connector)
                                (notification wire-notification)
                                (wire-schema  t))
  (let+ (((&structure-r/o connector- assembly-pool converter) connector)
         ((&structure-r/o wire-notification- buffer end) notification)
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
                          0 end)))

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
