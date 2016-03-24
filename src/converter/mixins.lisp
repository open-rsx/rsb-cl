;;;; mixins.lisp --- Mixin classes for converter classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

;;; `wire->domain-cache-mixin'

(defclass wire->domain-cache-mixin ()
  ((wire->domain-cache :type     hash-table
                       :reader   converter-%wire->domain-cache
                       :initform (make-hash-table :test #'eq)
                       :documentation
                       "Stores a cache for values returned by the
                        `wire->domain?' method."))
  (:documentation
   "This class is intended to be mixed into converter classes that
    perform expensive but cachable computations in the `wire->domain?'
    method.

    Caching is performed on the WIRE-SCHEMA argument using `eq'
    comparison."))

(defmethod wire->domain? :around ((converter   wire->domain-cache-mixin)
                                  (wire-data   t)
                                  (wire-schema t))
  (values-list (ensure-gethash
                wire-schema (converter-%wire->domain-cache converter)
                (multiple-value-list (call-next-method)))))

;;; `domain->wire-cache-mixin'

(defclass domain->wire-cache-mixin ()
  ((domain->wire-cache :type     hash-table
                       :reader   converter-%domain->wire-cache
                       :initform (make-hash-table :test #'eq)
                       :documentation
                       "Stores a cache for the values returned by the
                        `domain->wire?' method."))
  (:documentation
   "This class is intended to be mixed into converter classes that
    perform expensive but cachable computations in the `wire->domain?'
    method.

    Caching is performed on the `class-of' the DOMAIN-OBJECT argument
    using `eq' comparison."))

(defmethod domain->wire? :around ((converter     domain->wire-cache-mixin)
                                  (domain-object t))
  (values-list (ensure-gethash
                (class-of domain-object)
                (converter-%domain->wire-cache converter)
                (multiple-value-list (call-next-method)))))
