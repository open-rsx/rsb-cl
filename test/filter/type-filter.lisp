;;;; type-filter.lisp --- Unit tests for the type-filter class.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite type-filter-root (filter-root
                                filter-suite)
  ((simple-filter (make-instance 'type-filter :type t)))
  (:documentation
   "Unit tests for the `type-filter' class."))

(define-basic-filter-test-cases (type-filter :type)
    ;; Construct cases
    '(;; Some invalid cases.
      (()              error) ; missing initarg
      ((:type 5)       error) ; wrong type for type specifier


      ;; These are ok.
      ((:type t)       :ok)
      ((:type integer) :ok))

  ;; Expected matching results
  t t t t)
