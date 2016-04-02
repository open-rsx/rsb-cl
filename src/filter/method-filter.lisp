;;;; method-filter.lisp --- Event filtering based on method.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass method-filter (funcallable-filter-mixin
                         print-items:print-items-mixin)
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

(defmethod rsb.ep:access? ((processor method-filter)
                           (part      (eql :method))
                           (mode      (eql :read)))
  t)

(defmethod matches? ((filter method-filter) (event event))
  (eq (filter-method filter) (event-method event)))

(defmethod print-items:print-items append ((object method-filter))
  `((:method ,(filter-method object))))
