;;;; type-filter.lisp --- A filter that discriminates event based on their type.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass type-filter (funcallable-filter-mixin)
  ((type :initarg  :type
         :type     (or list symbol)
         :accessor filter-type
         :documentation
         "The type of matching events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :type (missing-required-initarg 'type-filter :type))
  (:documentation
   "Discriminate based on the type of event payloads."))

(service-provider:register-provider/class 'filter :type
  :class 'type-filter)

(defmethod matches? ((filter type-filter) (event event))
  (typep (event-data event) (filter-type filter)))

(defmethod print-object ((object type-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-type object))))
