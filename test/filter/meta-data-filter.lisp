;;;; meta-data-filter.lisp --- Unit tests for meta-data-filter class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(def-suite meta-data-filter-root
  :in filter-root
  :description
  "Test suite for the `meta-data-filter' class.")

(define-basic-filter-tests (meta-data-filter :meta-data)
  `((()                               error) ; missing initargs
    ((:key :foo)                      error) ; missing :predicate initarg
    ((:predicate 'emptyp)             error) ; missing :key initarg

    ;; these are ok
    ((:key :foo :predicate emptyp)    t)
    ((:key :foo :predicate ,#'emptyp) t)))

(define-filter-match-test (meta-data-filter :meta-data)
  `(((:key :foo :predicate ,(curry #'eql 1)) ("/" 1)        nil)
    ((:key :foo :predicate ,(curry #'eql 1)) ("/" 1 :foo 2) nil)
    ((:key :foo :predicate ,(curry #'eql 1)) ("/" 1 :foo 1) t)))
