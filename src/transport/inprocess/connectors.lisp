;;;; connectors.lisp --- Connectors of the inprocess transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

;;; `connector'

(defclass connector (rsb.transport:connector)
  ((transport :initarg  %transport
              :reader   connector-transport))
  (:metaclass connector-class)
  (:transport :inprocess)
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

(defclass in-connector (broadcast-processor
                        error-policy-handler-mixin
                        restart-handler-mixin
                        restart-dispatcher-mixin
                        connector)
  ()
  (:metaclass connector-class)
  (:direction :in)
  (:documentation
   "Receives events from the in-process bus."))

(register-connector :inprocess :in 'in-connector)

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

(defmethod handle :before ((connector in-connector)
                           (event     event))
  (setf (timestamp event :receive) (local-time:now)))

;;; `out-connector' class

(defclass out-connector (error-handling-sender-mixin
                         restart-handler-mixin
                         connector)
  ((scope-sinks :accessor connector-%scope-sinks
                :documentation
                "Caches the sink trie of the associated transport."))
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Send events to the in-process bus."))

(register-connector :inprocess :out 'out-connector)

(defmethod shared-initialize :after
    ((instance   out-connector)
     (slot-names t)
     &key
     ((%transport transport) nil transport-supplied?))
  (when transport-supplied?
    (setf (connector-%scope-sinks instance)
          (transport-scope-sinks transport))))

(defmethod handle :before ((connector out-connector)
                           (event     event))
  (setf (timestamp event :send) (local-time:now)))

(defmethod handle ((connector out-connector) (event event))
  (flet ((do-scope (scope connectors)
           (declare (ignore scope))
           (handle connectors event)))
    (declare (dynamic-extent #'do-scope))
    (rsb.ep:scope-trie-map
     #'do-scope (event-scope event) (connector-%scope-sinks connector))))
