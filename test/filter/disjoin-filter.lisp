;;;; disjoin-filter.lisp --- Unit test for the disjoin-filter class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite disjoin-filter-root (filter-root
                                   filter-suite)
  ((simple-filter (filter
                   '(:or
                     (:scope :scope "/foo/bar")
                     (:type :type string)))))
  (:documentation
   "Unit tests for the `disjoin-filter' class."))

(define-basic-filter-test-cases (disjoin-filter :or)
    ;; Construction
    '(;; these are ok
      (()              t)
      ((:children nil) t))

  ;; Matching results
  t t t nil)
