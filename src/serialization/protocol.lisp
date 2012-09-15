;;;; protocol.lisp ---
;;;;
;;;; Copyright (C) 2012, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.serialization)

;;; Event (de)serialization protocol

(defgeneric notification->event (serialization notifcation)
  (:documentation
   "Convert NOTIFICATION into an `event' instance and return the
    event. If message cannot be converted into an event, return nil
    instead. Signal a `decoding-error' if something goes wrong."))

(defgeneric event->notification (serialization event)
  (:documentation
   "Convert EVENT into a notification for sending via
    CONNECTOR. Return the notification. If EVENT cannot be converted
    into a notification, maybe return nil, depending on the error
    handling policy. Signal an `encoding-error' if something goes
    wrong."))

;; Default behavior

(defmethod notification->event :around ((serialization t)
                                        (notification  t))
  "TODO(jmoringe): document"
  (with-condition-translation
      (((error decoding-error)
        :encoded          (list notification) ;;; TODO(jmoringe): hack
        :format-control   "~@<After unpacking, the ~
                           notification~_~A~_could not be converted ~
                           into an event.~:@>"
        :format-arguments (list (with-output-to-string (stream)
                                  (describe notification stream)))))
    (call-next-method)))

(defmethod event->notification :around ((serialization t)
                                        (event         t))
  (with-condition-translation
      (((error encoding-error)
        :event            event
        :format-control   "~@<The event ~S could not be packed using ~
                           protocol buffer serialization.~@:>"
        :format-arguments (list event)))
    (call-next-method)))

;;; Service

(service-provider:define-service serialization
  (:documentation
   "Providers of this service implement RSB event serialization
    mechanisms."))
