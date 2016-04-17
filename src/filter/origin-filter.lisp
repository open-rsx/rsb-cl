;;;; origin-filter.lisp --- Event filtering based on origin id.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass origin-filter (funcallable-filter-mixin
                         print-items:print-items-mixin)
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
    (setf (filter-%origin instance) (ensure-uuid origin))))

(defmethod rsb.ep:access? ((processor origin-filter)
                           (part      (eql :origin))
                           (mode      (eql :read)))
  t)

(defmethod matches? ((filter origin-filter) (event event))
  (when-let ((event-origin (event-origin event)))
    (uuid:uuid= (filter-origin filter) (event-origin event))))

(defmethod print-items:print-items append ((object origin-filter))
  `((:origin ,(filter-origin object) "~/rsb::print-id/")))
