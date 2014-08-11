;;;; in-push-connector.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess)

(defmethod find-transport-class ((spec (eql :inprocess-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (broadcast-processor
                             error-policy-handler-mixin
                             restart-handler-mixin
                             restart-dispatcher-mixin
                             connector)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod notify ((connector in-push-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (log:debug "~@<~A is attaching to scope ~A~@:>" connector scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-push-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (log:debug "~@<~A is detaching from scope ~A~@:>" connector scope)
  (removef (by-scope scope) connector :count 1))

(defmethod handle :before ((connector in-push-connector)
                           (event     event))
  (setf (timestamp event :receive) (local-time:now)))
