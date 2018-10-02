;;;; in-connector.lisp --- Superclass for in-direction connector classes.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
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

(defmethod notify ((recipient in-connector)
                   (subject   scope)
                   (action    (eql :attached)))
  ;; Connect if necessary.
  (unless (connector-connection recipient)
    (notify recipient t :attached))

  (let+ (((&values &ign &ign promise)
          (ref-group (connector-connection recipient) (scope->group subject)
                     :waitable? t)))
    ;; If necessary, wait for the Spread group joining operation to
    ;; complete.
    (lparallel:force promise)))

(defmethod notify ((recipient in-connector)
                   (subject   scope)
                   (action    (eql :detached)))
  (let+ (((&values &ign group-count promise)
          (unref-group (connector-connection recipient) (scope->group subject)
                       :waitable? t)))
    ;; If necessary, wait for the Spread group leaving operation to
    ;; complete.
    (lparallel:force promise)
    ;; If this was the final reference to the final group of the
    ;; connection, detach RECIPIENT.
    (when (zerop group-count)
      (notify recipient t :detached))))

(defmethod notify ((recipient in-connector)
                   (subject   (eql t))
                   (action    (eql :detached)))
  (call-next-method)
  (detach (connector-assembly-pool recipient)))

(defmethod receive-notification ((connector in-connector)
                                 (block?    t))
  ;; Delegate receiving a notification to the connection of RECIPIENT.
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

    ;; Try to unpack NOTIFICATION into a `fragmented-notification'
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
                          0 end))

      ;; After unpacking, there are two possible cases:
      ;; 1. NOTIFICATION (maybe in conjunction with previously
      ;; received notifications) forms a complete event
      ;; 2. NOTIFICATION does not form a complete event. In this case,
      ;; return `nil'.
      (setf notification (or (assemble-notification assembly-pool notification)
                             (return-from notification->event nil))))

    ;; Convert the `incoming-notification' instance NOTIFICATION,
    ;; and its payload, into an `event' instance.
    ;; * If the payload conversion succeeds, return the `event'
    ;;   instance.
    ;; * If the payload conversion fails, signal an appropriate error.
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
      (let+ (((&structure-r/o incoming-notification- notification wire-data)
              notification))
        (one-notification->event converter notification wire-data
                                 :expose-wire-schema?  expose-wire-schema?
                                 :expose-payload-size? expose-payload-size?)))))
