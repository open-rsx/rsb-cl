;;;; origin-filter.lisp --- Event filtering based on origin id.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defmethod find-filter-class ((spec (eql :origin)))
  (find-class 'origin-filter))

(defclass origin-filter (filter-mixin)
  ((origin :type     uuid:uuid
           :accessor filter-origin
           :documentation
           ""))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :origin (missing-required-initarg 'origin-filter :origin))
  (:documentation
   "This filter discriminates based on the origin id of events."))

(defmethod shared-initialize :after ((instance   origin-filter)
                                     (slot-names t)
                                     &key
                                     origin)
  (setf (slot-value instance 'origin)
        (etypecase origin
          (uuid:uuid            origin)
          (string               (uuid:make-uuid-from-string origin))
          (nibbles:octet-vector (uuid:byte-array-to-uuid origin)))))

(defmethod matches? ((filter origin-filter) (event event))
  (uuid:uuid= (filter-origin filter) (event-origin event)))

(defmethod print-object ((object origin-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/rsb::print-id/" (filter-origin object))))
