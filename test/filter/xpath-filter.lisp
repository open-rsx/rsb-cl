;;;; xpath-filter.lisp --- Unit tests for the xpath-filter class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite xpath-filter-root (filter-root
                                 filter-suite)
  ((simple-filter (make-instance 'xpath-filter
                                 :xpath "*")))
  (:documentation
   "Unit tests for the `xpath-filter' class."))

(define-basic-filter-test-cases (xpath-filter :xpath)
    ;; Construct cases
    '(;; Some invalid cases.
      (()                          error) ; missing initarg
      ((:xpath 5)                  error) ; wrong type for XPath
      ((:xpath ".*")               error) ; invalid XPath

      ;; These are ok.
      ((:xpath "*")                :ok)
      ((:xpath "*" :always :match) :ok))

  ;; Expected matching results
  t t t t)
