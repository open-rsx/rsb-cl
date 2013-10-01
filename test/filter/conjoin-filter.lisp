;;;; conjoin-filter.lisp --- Unit test for the conjoin-filter class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite conjoin-filter-root (filter-root
                                   filter-suite)
  ((simple-filter (filter
                   '((:and
                      (:type :type string)
                      (:scope :scope "/foo"))))))
  (:documentation
   "Unit tests for the `conjoin-filter' class."))

(define-basic-filter-test-cases (conjoin-filter :and)
    ;; construct cases
    '(;; these are ok
      (() t))

  ;; expected matching results
  nil t nil nil)
