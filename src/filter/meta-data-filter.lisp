;;;; meta-data-filter.lisp --- Event filtering based on meta-data items.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass meta-data-filter (function-caching-mixin
                            funcallable-filter-mixin
                            print-items:print-items-mixin)
  ((key       :initarg  :key
              :type     keyword
              :reader   filter-key
              :documentation
              "The key of the meta-data item on which the filter should
               operate.")
   (predicate :type     function
              :reader   filter-predicate
              :writer   (setf filter-%predicate)
              :documentation
              "A function called on the value of the meta-data item
               named by the value of the `key' slot to determine
               whether an should match the filter."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :key       (missing-required-initarg 'meta-data-filter :key)
   :predicate (missing-required-initarg 'meta-data-filter :predicate))
  (:documentation
   "This filter discriminates based on the value of one meta-data item."))

(service-provider:register-provider/class
 'filter :meta-data :class 'meta-data-filter)

(defmethod shared-initialize :after ((instance   meta-data-filter)
                                     (slot-names t)
                                     &key
                                     (predicate nil predicate-supplied?))
  (when predicate-supplied?
    (setf (filter-%predicate instance) (coerce predicate 'function))))

(defmethod rsb.ep:access? ((processor meta-data-filter)
                           (part      (eql :meta-data))
                           (mode      (eql :read)))
  t)

(defmethod compute-filter-function ((filter meta-data-filter) &key next)
  (declare (ignore next))
  (let+ (((&structure-r/o filter- key predicate) filter))
    (declare (type function predicate))
    (lambda (event)
      (funcall predicate (meta-data event key)))))

(defmethod print-items:print-items append ((object meta-data-filter))
  (let+ (((&structure-r/o filter- key predicate) object))
    `((:predicate-and-key (,predicate ,key) "~{(~A ~S)~}"))))
