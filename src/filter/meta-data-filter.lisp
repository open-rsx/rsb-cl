;;;; meta-data-filter.lisp --- Event filtering based on meta-data items.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass meta-data-filter (funcallable-filter-mixin)
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

(defmethod rsb.ep:access? ((transform meta-data-filter)
                           (part      (eql :meta-data))
                           (mode      (eql :read)))
  t)

(defmethod matches? ((filter meta-data-filter) (event event))
  (let+ (((&structure-r/o filter- key predicate) filter)
         (value (meta-data event key)))
    (funcall (the function predicate) value)))

(defmethod print-object ((object meta-data-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let+ (((&structure-r/o filter- key predicate) object))
      (format stream "(~A ~S)" predicate key))))
