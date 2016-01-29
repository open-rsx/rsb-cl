;;;; connectors.lisp --- Connectors of the inprocess transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

;;; `connector'

(defclass connector (rsb.transport:connector)
  ((transport :initarg  %transport
              :reader   connector-transport))
  (:metaclass connector-class)
  (:transport :inprocess)
  (:schemas   :inprocess)
  (:default-initargs
   :schema :inprocess)
  (:documentation
   "Superclass for connector classes of the inprocess transport."))

(defmethod shared-initialize :around ((instance   connector)
                                      (slot-names t)
                                      &rest args &key)
  (let ((transport (connector-transport (class-of instance))))
    (apply #'call-next-method instance slot-names
           '%transport transport
           :host       (transport-machine-instance transport)
           :port       (transport-process-id       transport)
           args)))

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
  (let+ (((&structure-r/o transport- scope-sinks)
          (connector-transport connector)))
    (log:debug "~@<~A is attaching to scope ~A~@:>" connector scope)
    (rsb.ep:sink-scope-trie-add scope-sinks scope connector)
    (log:debug "~@<Scope trie after adding ~A:~@:_~/rsb.ep::print-trie/~@:>"
               connector scope-sinks)))

(defmethod notify ((connector in-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (let+ (((&structure-r/o transport- scope-sinks)
          (connector-transport connector)))
    (log:debug "~@<~A is detaching from scope ~A~@:>" connector scope)
    (rsb.ep:sink-scope-trie-remove scope-sinks scope connector)
    (log:debug "~@<Scope trie after removing ~A:~@:_~/rsb.ep::print-trie/~@:>"
               connector scope-sinks)))

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

(register-connector :inprocess :in-pull 'in-pull-connector)

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

(register-connector :inprocess :in-push 'in-push-connector)

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

(register-connector :inprocess :out 'out-connector)

(defmethod handle :before ((connector out-connector)
                           (event     event))
  (setf (timestamp event :send) (local-time:now)))

(defmethod handle ((connector out-connector) (event event))
  (let+ (((&structure-r/o transport- scope-sinks)
          (connector-transport connector))
         ((&flet do-scope (connectors)
            (handle connectors event))))
    (declare (dynamic-extent #'do-scope))
    (rsb.ep:scope-trie-map #'do-scope (event-scope event) scope-sinks)))
