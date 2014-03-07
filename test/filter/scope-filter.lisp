;;;; scope-filter.lisp --- Unit tests for the scope-filter class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite scope-filter-root (filter-root
                                 filter-suite)
  ((simple-filter (make-filter :scope :scope "/foo")))
  (:documentation
   "Unit tests for the `scope-filter' class."))

(define-basic-filter-test-cases (scope-filter :scope)
    ;; construct cases
    '(;; errors
      (()              error) ; missing required initargs
      ((:scope "<>!")  error) ; invalid scope

      ;; these are ok
      ((:scope "/foo") t))

    nil t t t)
