;;;; method-filter.lisp --- Event filtering based on method.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.filter)

(defmethod find-filter-class ((spec (eql :method)))
  (find-class 'method-filter))

(defclass method-filter (filter-mixin)
  ((method :initarg  :method
	   :type     (or null string)
	   :reader   filter-method
	   :documentation
	   "Stores the method name to which the filter should restrict
events. Valid values are either strings which match events with
identical method strings or NIL which matches events without
methods."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :method (missing-required-initarg 'method-filter :method))
  (:documentation
   "This filter discriminates based on the method of events. Valid
method values are either strings which match events with identical
method strings or NIL which matches events without methods."))

(defmethod matches? ((filter method-filter) (event event))
  (let ((filter-method (filter-method filter))
	(event-method  (event-method event)))
    (case filter-method
      ((nil) (not event-method))
      (t     (and event-method
		  (string= event-method filter-method))))))

(defmethod print-object ((object method-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-method object))))
