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
                        expose-transport-metrics-mixin)
  ()
  (:metaclass connector-class)
  (:documentation
   "This class is intended to be used as a superclass of in-direction
    connector classes for Spread."))

(defmethod notify ((recipient in-connector)
                   (subject   scope)
                   (action    (eql :attached)))
  (call-next-method)
  (notify (bus recipient) recipient (subscribed subject)))

(defmethod notify ((recipient in-connector)
                   (subject   scope)
                   (action    (eql :detached)))
  (notify (bus recipient) recipient (unsubscribed subject))
  (call-next-method))

(defmethod notification->event ((connector    in-connector)
                                (notification bus-notification)
                                (wire-schema  t))
  (let+ (((&structure-r/o connector- converter) connector)
         ((&structure-r/o bus-notification- notification wire-data) notification)
         (expose-wire-schema?  (connector-expose? connector :rsb.transport.wire-schema))
         (expose-payload-size? (connector-expose? connector :rsb.transport.payload-size)))

    ;; Convert the `bus-notification' instance NOTIFICATION, and its
    ;; payload, into an `event' instance.
    ;; * If the payload conversion succeeds, return the `event'
    ;;   instance.
    ;; * If the payload conversion fails, signal an appropriate error.
    (with-condition-translation
        (((error decoding-error)
          :encoded          notification
          :format-control   "~@<After assembling and unpacking, the ~
                             notification~_~A~_could not be converted ~
                             into an event.~:@>"
          :format-arguments `(,(with-output-to-string (stream)
                                 (describe notification stream)))))
      (one-notification->event converter notification wire-data
                               :expose-wire-schema?  expose-wire-schema?
                               :expose-payload-size? expose-payload-size?))))
