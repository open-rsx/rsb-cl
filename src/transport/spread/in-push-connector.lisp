;;;; in-push-connector.lisp --- An in-direction, push-based connector for spread.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `in-push-connector' class

(defmethod find-transport-class ((spec (eql :spread-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (in-connector
                             threaded-message-receiver-mixin
                             error-handling-push-receiver-mixin)
  ((state :type     (member :inactive :active :shutdown)
          :accessor connector-%state
          :initform :active
          :documentation
          "Stores the state of the connector. Currently only used
           during shutdown."))
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "This connector class implements push-style event receiving for the
Spread transport."))

(defmethod apply-error-policy ((processor in-push-connector)
                               (condition connection-unexpectedly-closed))
  (if (eq (connector-%state processor) :shutdown)
      (exit-receiver)
      (call-next-method)))

(defmethod apply-error-policy ((processor in-push-connector)
                               (condition network.spread:spread-error))
  (if (eq (connector-%state processor) :shutdown)
      (exit-receiver)
      (call-next-method)))

(defmethod notify ((connector in-push-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  ;; When attaching to SCOPE, start a receiver thread.
  (call-next-method)
  (setf (connector-%state connector) :active)
  (start-receiver connector))

(defmethod notify ((connector in-push-connector)
                   (scope     (eql t))
                   (action    (eql :detached)))
  ;; When detaching from SCOPE, join the receiver thread.
  (setf (connector-%state connector) :shutdown)
  (call-next-method)
  (stop-receiver connector)
  (setf (connector-%state connector) :inactive))

(defmethod receive-messages :around ((connector in-push-connector))
  "Create a thread-local scope->groups cache for this connector."
  (let ((*scope->groups-cache* (make-scope->groups-cache)))
    (call-next-method)))
