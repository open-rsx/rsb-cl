;;;; composite-filter.lisp --- Composite filters.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; Class `complement-filter'

(defclass complement-filter (composite-filter-mixin
                             function-caching-mixin
                             funcallable-filter-mixin
                             print-items:print-items-mixin)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :children (missing-required-initarg 'complement-filter :children))
  (:documentation
   "Instances of this class make filtering decisions by forming the
    logical complement of the decisions made by their single
    subordinate filter."))

(service-provider:register-provider/class
 'filter :complement :class 'complement-filter)

(service-provider:register-provider/class
 'filter :not :class 'complement-filter)

(defmethod shared-initialize :before ((instance   complement-filter)
                                      (slot-names t)
                                      &key
                                      (children '() children-supplied?))
  (unless (or (not children-supplied?) (length= 1 children))
    (error "~@<~A can only have a single subordinate filter (not ~D).~@:>"
           (class-of instance) (length children))))

(defmethod compute-filter-function ((filter complement-filter) &key next)
  (declare (ignore next))
  (let ((function (%maybe-filter-function (first (filter-children filter)))))
    (declare (type function function))
    (lambda (event)
      (not (funcall function event)))))

;;; Classes `conjoin-filter' and `disjoin-filter'

(macrolet
    ((define-composite-filter ((name &rest designators)
                               operation-name
                               empty-value
                               short-circuit-condition short-circuit-value)
       `(progn
          (defclass ,name (composite-filter-mixin
                           function-caching-mixin
                           funcallable-filter-mixin
                           print-items:print-items-mixin)
            ()
            (:metaclass closer-mop:funcallable-standard-class)
            (:documentation
             ,(format nil "Instances of this class make filtering ~
                           decisions by forming the logical ~A of the ~
                           decisions made by their subordinate ~
                           filters."
                      operation-name)))

          ,@(mapcar
             (lambda (designator)
               `(service-provider:register-provider/class 'filter ,designator
                                                          :class ',name))
             designators)

          (defmethod compute-filter-function ((filter ,name) &key next)
            (declare (ignore next))
            (let ((children (filter-children filter)))
              (case (length children)
                (0
                 (constantly ,empty-value))
                (1
                 (%maybe-filter-function (first children)))
                (t
                 (let ((functions (mapcar #'%maybe-filter-function children)))
                   (named-lambda match (event)
                     (loop :for function :of-type function :in functions
                            ,short-circuit-condition (funcall function event)
                           :do (return-from match ,short-circuit-value))
                     ,empty-value)))))))))

  (define-composite-filter (conjoin-filter :conjoin :and)
    "and" t   :unless nil)
  (define-composite-filter (disjoin-filter :disjoin :or)
    "or"  nil :when   t))

;;; Utilities functions

(defun %maybe-filter-function (child)
  (cond
    ((compute-applicable-methods
      #'filter-function (list child))
     (filter-function child))
    ((functionp child)
     child)
    (t
     (curry #'matches? child))))
