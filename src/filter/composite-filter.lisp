;;;; composite-filter.lisp --- Composite filter classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass composite-filter (filter-mixin)
  ((children :initarg  :children
             :type     list
             :accessor filter-children
             :initform '()
             :documentation
             "A list of subordinate filters."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of subclasses of this class implement complex filtering
behavior by combining decisions of a set of subordinate filters. On
rare occasions is it useful to make instances of this class itself
rather than subclasses."))

(defmethod print-object ((object composite-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (filter-children object)))))

;;; Class `complement-filter'

(defmethod find-filter-class ((spec (eql :complement)))
  (find-class 'complement-filter))

(defmethod find-filter-class ((spec (eql :not)))
  (find-class 'complement-filter))

(defclass complement-filter (composite-filter)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :children (missing-required-initarg 'complement-filter :children))
  (:documentation
   "Instances of this class make filtering decisions by forming the
    logical complement of the decisions made by their single
    subordinate filter."))

(defmethod shared-initialize :before ((instance   complement-filter)
                                      (slot-names t)
                                      &key
                                      (children '() children-supplied?))
  (unless (or (not children-supplied?) (length= 1 children))
    (error "~@<~A can only have a single subordinate filter (not ~D).~@:>"
           (class-of instance) (length children))))

(defmethod matches? ((filter complement-filter) (event t))
  (not (matches? (first (filter-children filter)) event)))

;;; Class `conjoin-filter'

(defmethod find-filter-class ((spec (eql :conjoin)))
  (find-class 'conjoin-filter))

(defmethod find-filter-class ((spec (eql :and)))
  (find-class 'conjoin-filter))

(defclass conjoin-filter (composite-filter)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class make filtering decisions by forming the
logical and of the decisions made by their subordinate filters."))

(defmethod matches? ((filter conjoin-filter) (event t))
  (every (rcurry #'matches? event) (filter-children filter)))

;;; Class `disjoin-filter'

(defmethod find-filter-class ((spec (eql :disjoin)))
  (find-class 'disjoin-filter))

(defmethod find-filter-class ((spec (eql :or)))
  (find-class 'disjoin-filter))

(defclass disjoin-filter (composite-filter)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class make filtering decisions by forming the
logical or of the decisions made by their subordinate filters."))

(defmethod matches? ((filter disjoin-filter) (event t))
  (some (rcurry #'matches? event) (filter-children filter)))
