;;;; origin-filter.lisp --- Event filtering based on origin id.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass origin-filter (funcallable-filter-mixin)
  ((origin :type     uuid:uuid
           :reader   filter-origin
           :writer   (setf filter-%origin)
           :documentation
           "Stores the origin id to which the filter should restrict
            events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :origin (missing-required-initarg 'origin-filter :origin))
  (:documentation
   "This filter discriminates based on the origin id of events."))

(service-provider:register-provider/class 'filter :origin
  :class 'origin-filter)

(defmethod shared-initialize :after ((instance   origin-filter)
                                     (slot-names t)
                                     &key
                                     (origin nil origin-supplied?))
  (when origin-supplied?
    (setf (filter-origin instance) origin)))

(defgeneric (setf filter-origin) (new-value filter)
  (:method ((new-value uuid:uuid) (filter origin-filter))
    (setf (filter-%origin filter) new-value))
  (:method ((new-value string) (filter origin-filter))
    (setf (filter-%origin filter) (uuid:make-uuid-from-string new-value)))
  (:method ((new-value simple-array) (filter origin-filter))
    (if (typep new-value 'nibbles:octet-vector)
        (setf (filter-%origin filter) (uuid:byte-array-to-uuid new-value))
        (call-next-method))))

(defmethod rsb.ep:access? ((processor origin-filter)
                           (part      (eql :origin))
                           (mode      (eql :read)))
  t)

(defmethod matches? ((filter origin-filter) (event event))
  (uuid:uuid= (filter-origin filter) (event-origin event)))

(defmethod print-object ((object origin-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/rsb::print-id/" (filter-origin object))))
