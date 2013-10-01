;;;; in-push-connector.lisp --- An in-direction, push-based connector for spread.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.spread)

;;; `in-push-connector' class

(defmethod find-transport-class ((spec (eql :spread-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (in-connector
                             threaded-message-receiver-mixin
                             error-handling-push-receiver-mixin
                             sometimes-interruptible-mixin)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "This connector class implements push-style event receiving for the
Spread transport."))

(defmethod notify :after ((connector in-push-connector)
                          (scope     scope)
                          (action    (eql :attached)))
  "After attaching to SCOPE, start a receiver thread."
  (start-receiver connector))

(defmethod notify :before ((connector in-push-connector)
                           (scope     scope)
                           (action    (eql :detached)))
  "Before detaching from SCOPE, join the receiver thread."
  (stop-receiver connector))

(defmethod receive-messages :around ((connector in-push-connector))
  "Create a thread-local scope->groups cache for this connector."
  (let ((*scope->groups-cache* (make-scope->groups-cache)))
    (call-next-method)))
