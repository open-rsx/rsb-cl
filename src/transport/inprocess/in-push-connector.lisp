;;;; in-push-connector.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

(defmethod find-transport-class ((spec (eql :inprocess-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (connector
                             broadcast-processor
                             error-handling-dispatcher-mixin
                             error-handling-push-receiver-mixin)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod notify ((connector in-push-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (log:info "~@<~A is attaching to scope ~A~@:>" connector scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-push-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (log:info "~@<~A is detaching from scope ~A~@:>" connector scope)
  (removef (by-scope scope) connector :count 1))

(defmethod handle :before ((connector in-push-connector)
                           (event     event))
  (setf (timestamp event :receive) (local-time:now)))
