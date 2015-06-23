;;;; connectors.lisp --- inprocess connector classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

;;; Global scope -> in-connector mapping

(#+sbcl sb-ext:defglobal #-sbcl defvar **by-scope** (rsb.ep:make-sink-scope-trie)
  "Association of scopes to event sinks interested in the respective
   scopes.")

(declaim #+sbcl (sb-ext:always-bound **by-scope**)
         (type rsb.ep:sink-scope-trie **by-scope**))

;;; Global cache variables

(#+sbcl sb-ext:defglobal #-sbcl defvar **cached-machine-instance**
  (machine-instance))

(#+sbcl sb-ext:defglobal #-sbcl defvar **cached-process-id**
  (sb-posix:getpid))

#+sbcl (declaim (sb-ext:always-bound **cached-machine-instance**
                                     **cached-process-id**))

(defun update-cached-machine-instance-and-process-id ()
  (setf **cached-machine-instance** (machine-instance)
        **cached-process-id**       (sb-posix:getpid)))

#+sbcl (pushnew 'update-cached-machine-instance-and-process-id
                sb-ext:*init-hooks*)
#-sbcl (pushnew 'update-cached-machine-instance-and-process-id
                uiop:*image-restore-hook*)

;;; `connector'

(defclass connector (rsb.transport:connector)
  ()
  (:metaclass connector-class)
  (:default-initargs
   :schema :inprocess
   :host   **cached-machine-instance**
   :port   **cached-process-id**)
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
  (rsb.ep:sink-scope-trie-add **by-scope** scope connector)
  (log:debug "~@<Scope trie after adding ~A:~@:_~/rsb.ep::print-trie/~@:>"
             connector **by-scope**))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (log:debug "~@<~A is detaching from scope ~A~@:>" connector scope)
  (rsb.ep:sink-scope-trie-remove **by-scope** scope connector)
  (log:debug "~@<Scope trie after removing ~A:~@:_~/rsb.ep::print-trie/~@:>"
             connector **by-scope**))

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
  (flet ((do-scope (connectors)
           (handle connectors event)))
    (declare (dynamic-extent #'do-scope))
    (rsb.ep:scope-trie-map #'do-scope (event-scope event) **by-scope**)))
