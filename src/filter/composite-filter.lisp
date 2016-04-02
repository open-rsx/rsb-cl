;;;; composite-filter.lisp --- Composite filters.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; Class `composite-filter'

(defclass composite-filter (funcallable-filter-mixin)
  ((children :initarg  :children
             :type     list
             :accessor filter-children
             :initform '()
             :documentation
             "A list of subordinate filters."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of subclasses of this class implement complex filtering
    behavior by combining decisions of a set of subordinate
    filters. On rare occasions is it useful to make instances of this
    class itself rather than subclasses."))

(defmethod rsb.ep:access? ((processor composite-filter)
                           (part      t)
                           (mode      t))
  (rsb.ep:access? (filter-children processor) part mode))

(defmethod print-object ((object composite-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (filter-children object)))))

;;; Class `complement-filter'

(defclass complement-filter (composite-filter)
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

(defmethod matches? ((filter complement-filter) (event t))
  (not (matches? (first (filter-children filter)) event)))

;;; Classes `conjoin-filter' and `disjoin-filter'

(macrolet
    ((define-composite-filter ((name &rest designators)
                               operation operation-name)
       `(progn
          (defclass ,name (composite-filter)
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

          (defmethod matches? ((filter ,name) (event t))
            (,operation (rcurry #'matches? event) (filter-children filter))))))

  (define-composite-filter (conjoin-filter :conjoin :and)
    every "and")
  (define-composite-filter (disjoin-filter :disjoin :or)
    some "or"))
