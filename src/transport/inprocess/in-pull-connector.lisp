;;;; in-pull-connector.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

;;; Interface for the in-direction, pull-based connector

(defgeneric connector-queue-count (connector)
  (:documentation
   "Return the number of notifications currently queued in CONNECTOR."))

;;; `in-pull-connector' class

(defmethod find-transport-class ((spec (eql :inprocess-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (broadcast-processor
                             error-handling-pull-receiver-mixin
                             restart-dispatcher-mixin
                             connector)
  ((queue :type     lparallel.queue:queue
          :reader   connector-queue
          :initform (lparallel.queue:make-queue)
          :documentation
          "Stores events as they arrive via the message bus."))
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod notify ((connector in-pull-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (log:debug "~@<~A is attaching to scope ~A~@:>" connector scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-pull-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (log:debug "~@<~A is detaching from scope ~A~@:>" connector scope)
  (removef (by-scope scope) connector :count 1))

(defmethod handle ((connector in-pull-connector)
                   (event     event))
  "Put EVENT into the queue maintained by CONNECTOR."
  (lparallel.queue:push-queue event (connector-queue connector)))

(defmethod receive-notification ((connector in-pull-connector)
                                 (block?    (eql nil)))
  ;; Extract and return one event from the queue maintained by
  ;; CONNECTOR, if there are any. If there are no queued events,
  ;; return nil.
  (lparallel.queue:try-pop-queue (connector-queue connector)))

(defmethod receive-notification ((connector in-pull-connector)
                                 (block?    t))
  ;; Extract and return one event from the queue maintained by
  ;; CONNECTOR, if there are any. If there are no queued events,
  ;; block.
  (lparallel.queue:pop-queue (connector-queue connector)))

(defmethod emit ((connector in-pull-connector) (block? t))
  (when-let ((event (receive-notification connector block?)))
    (setf (timestamp event :receive) (local-time:now))
    (dispatch connector event)
    t))

(defmethod print-object ((object in-pull-connector) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A ~A (~D)"
            (connector-direction object)
            (connector-relative-url object "/")
            (lparallel.queue:queue-count
             (connector-queue object)))))
