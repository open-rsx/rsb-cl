;;;; caching.lisp --- Caches {wire->domain,domain->wire}? and delegates.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

(defclass caching-converter (wire->domain-cache-mixin
                             domain->wire-cache-mixin)
  ((target :initarg  :target
           :reader   converter-target
           :documentation
           "Stores the target converter for which
            {wire->domain,domain->wire}? queries should be cached."))
  (:default-initargs
   :target (missing-required-initarg 'caching-converter :target))
  (:documentation
   "Caches {wire->domain,domain->wire}? queries to a target converter."))

(service-provider:register-provider/class
 'rsb.converter::converter :caching :class 'caching-converter)

(defmethod wire->domain? ((converter   caching-converter)
                          (wire-data   t)
                          (wire-schema t))
  (wire->domain? (converter-target converter) wire-data wire-schema))

(defmethod domain->wire? ((converter     caching-converter)
                          (domain-object t))
  (domain->wire? (converter-target converter) domain-object))

;;; The following two methods are provided for convenience and should
;;; not be called normally: `wire->domain?' and `domain->wire?' are
;;; supposed to be called first and return the converter that should
;;; be used in the `wire->domain' and `domain->wire' calls. This
;;; returned converter will never be the caching converter but the
;;; target converter (or another converter the target converter
;;; delegates to).

(defmethod wire->domain ((converter   caching-converter)
                         (wire-data   t)
                         (wire-schema t))
  (let ((converter (or (wire->domain? converter wire-data wire-schema)
                       (converter-target converter))))
    (wire->domain converter wire-data wire-schema)))

(defmethod domain->wire ((converter     caching-converter)
                         (domain-object t))
  (let ((converter (or (domain->wire? converter domain-object)
                       (converter-target converter))))
    (domain->wire converter domain-object)))
