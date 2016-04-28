;;;; in-push-connector.lisp --- An in-direction, push-based connector for spread.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `in-push-connector' class

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
   "Push-style event receiving for the Spread transport."))

(register-connector :spread :in-push 'in-push-connector)

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

(defmethod notify ((recipient in-push-connector)
                   (subject   (eql t))
                   (action    (eql :attached)))
  ;; When attaching, start a receiver thread.
  (call-next-method)
  (setf (connector-%state recipient) :active)
  (start-receiver recipient))

(defmethod notify ((recipient in-push-connector)
                   (scope     (eql t))
                   (action    (eql :detached)))
  ;; When detaching, join the receiver thread.
  (setf (connector-%state recipient) :shutdown)
  (call-next-method)
  (stop-receiver recipient)
  (setf (connector-%state recipient) :inactive))
