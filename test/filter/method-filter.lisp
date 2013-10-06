;;;; method-filter.lisp --- Unit tests for the method-filter class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite method-filter-root (filter-root
                                  filter-suite)
  ((simple-filter (make-instance 'method-filter
                                 :method :FOO)))
  (:documentation
   "Test suite for the `method-filter' class."))

(define-basic-filter-test-cases (method-filter :method)
    ;; Construct cases
    '(;; missing :method initarg
      (()                  :error)

      ;; these are ok
      ((:method nil)       t)
      ((:method :method)   t)
      ((:method :|method|) t))

  ;; Expected matching results
  nil nil nil nil)
