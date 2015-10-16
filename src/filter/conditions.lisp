;;;; conditions.lisp --- Conditions used in the filter module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(define-condition filter-construction-error (rsb-error
                                             chainable-condition)
  ((spec :initarg  :spec
         :type     list
         :reader   filter-construction-error-spec
         :documentation
         "The filter specification for which the attempt to construct
          a filter instance failed."))
  (:default-initargs
   :spec (missing-required-initarg 'filter-construction-error :spec))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to construct filter based on ~
                     specification ~
                     ~S~/more-conditions:maybe-print-cause/~@:>"
             (filter-construction-error-spec condition)
             condition)))
  (:documentation
   "This error is signaled when an attempt to construct a filter
    instance based on a filter specification fails."))
