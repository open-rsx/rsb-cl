;;;; method-filter.lisp --- Event filtering based on method.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass method-filter (funcallable-filter-mixin)
  ((method :initarg  :method
           :type     (or null keyword)
           :reader   filter-method
           :documentation
           "Stores the method name to which the filter should restrict
            events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :method (missing-required-initarg 'method-filter :method))
  (:documentation
   "This filter discriminates based on the method of events.

    Valid method values are either strings which match events with
    identical method strings or NIL which matches events without
    methods."))

(service-provider:register-provider/class 'filter :method
  :class 'method-filter)

(defmethod matches? ((filter method-filter) (event event))
  (eq (filter-method filter) (event-method event)))

(defmethod print-object ((object method-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-method object))))
