;;;; connectors.lisp --- inprocess connector classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

;;;

(defvar *by-scope* (make-hash-table :test     #'equal
                                    ;:weakness :value
                                    )
  "Association of scopes to event sinks interested in the respective
scopes.")

(defun by-scope (scope)
  "Return a list of connectors that are associated to SCOPE."
  (let ((key (%scope->key scope)))
    (gethash key *by-scope*)))

(defun (setf by-scope) (new-value scope)
  "Set the of handlers associated to SCOPE to NEW-VALUE."
  (let ((key (%scope->key scope)))
    (setf (gethash key *by-scope*) new-value)))

;;; `connector'

(defclass connector (rsb.transport:connector)
  ()
  (:metaclass connector-class)
  (:default-initargs
   :schema :inprocess
   :host   (load-time-value (machine-instance) t)
   :port   (load-time-value (sb-posix:getpid) t))
  (:wire-type t) ; The Lisp process is the medium, so t (any Lisp
                 ; object) should be a reasonable wire-type
  (:schemas   :inprocess)
  (:documentation
   "Superclass for connector classes of the inprocess transport."))

;;; `in-connector'

(defclass in-connector (connector)
  ()
  (:metaclass connector-class)
  (:documentation
   "Superclass for in-direction connector classes of the inprocess
    transport."))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (log:debug "~@<~A is attaching to scope ~A~@:>" connector scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (log:debug "~@<~A is detaching from scope ~A~@:>" connector scope)
  (removef (by-scope scope) connector :count 1))

;;; `in-pull-connector' class

(defmethod find-transport-class ((spec (eql :inprocess-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (broadcast-processor
                             error-handling-pull-receiver-mixin
                             restart-dispatcher-mixin
                             in-connector)
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

(defmethod handle ((connector in-pull-connector)
                   (event     event))
  ;; Put EVENT into the queue maintained by CONNECTOR.
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

;;; `in-push-connector' class

(defmethod find-transport-class ((spec (eql :inprocess-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (broadcast-processor
                             error-policy-handler-mixin
                             restart-handler-mixin
                             restart-dispatcher-mixin
                             in-connector)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "Instances of this connector class deliver RSB events within a
    process."))

(defmethod handle :before ((connector in-push-connector)
                           (event     event))
  (setf (timestamp event :receive) (local-time:now)))

;;; `out-connector' class

(defmethod find-transport-class ((spec (eql :inprocess-out)))
  (find-class 'out-connector))

(defclass out-connector (error-handling-sender-mixin
                         restart-handler-mixin
                         connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod handle :before ((connector out-connector)
                           (event     event))
  (setf (timestamp event :send) (local-time:now)))

(defmethod handle ((connector out-connector) (event event))
  (iter (for super in (super-scopes (event-scope event)
                                    :include-self? t))
        (handle (by-scope super) event)))

;;; Utility functions

(defun %scope->key (scope)
  "Convert the URI object URI into a scope string. "
  (scope-string scope))
