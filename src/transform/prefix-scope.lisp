;;;; prefix-scope.lisp --- Transform that add a prefix to scopes of transformed events.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform)

(defclass prefix-scope (funcallable-transform-mixin
                        print-items:print-items-mixin)
  ((prefix :type     scope
           :reader   transform-prefix
           :writer   (setf transform-%prefix)
           :documentation
           "Stores the prefix that should be added to the scopes of
            transformed events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :prefix (missing-required-initarg 'prefix-scope :prefix))
  (:documentation
   "Adds a prefix to scopes of transformed events."))

(service-provider:register-provider/class
 'transform :prefix-scope :class 'prefix-scope)

(defmethod shared-initialize :after ((instance   prefix-scope)
                                     (slot-names t)
                                     &key
                                     (prefix '() prefix-supplied?))
  (when prefix-supplied?
    (setf (transform-prefix instance) prefix)))

(defgeneric (setf transform-prefix) (new-value object)
  (:method ((new-value t) (object prefix-scope))
    (setf (transform-%prefix object) (make-scope new-value))
    new-value))

(defmethod transform! ((transform prefix-scope) (event event))
  (let+ (((&structure-r/o transform- prefix) transform))
    (setf (event-scope event) (merge-scopes (event-scope event) prefix)))
  event)

(defmethod print-items:print-items append ((object prefix-scope))
  `((:prefix ,(scope-string (transform-prefix object)) "~A")))
