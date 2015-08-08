;;;; meta-data-filter.lisp --- Unit tests for meta-data-filter class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite meta-data-filter-root (filter-root filter-suite)
  ((simple-filter (filter :meta-data
                          :key       :foo
                          :predicate (curry #'eql 1))))
  (:documentation
   "Test suite for the `meta-data-filter' class."))

(define-basic-filter-test-cases (meta-data-filter :meta-data)
  ;; Construct cases
  `((()                               error) ; missing initargs
    ((:key :foo)                      error) ; missing :predicate initarg
    ((:predicate 'emptyp)             error) ; missing :key initarg

    ;; these are ok
    ((:key :foo :predicate emptyp)    t)
    ((:key :foo :predicate ,#'emptyp) t))

  ;; Expected matching results
  nil nil nil t)
