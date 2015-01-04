;;;; regex-filter.lisp --- Unit tests for the regex-filter class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite regex-filter-root (filter-root
                                 filter-suite)
  ((simple-filter (make-instance 'regex-filter
                                 :regex ".*")))
  (:documentation
   "Unit tests for the `regex-filter' class."))

(define-basic-filter-test-cases (regex-filter :regex)
    ;; Construct cases
    '(;; Some invalid cases.
      (()                               error) ; missing initarg
      ((:regex 5)                       error) ; wrong type for regex
      ((:regex "*")                     error) ; invalid regex


      ;; These are ok.
      ((:regex ".*")                    :ok)
      ((:regex ".*" :always :match)     :ok)
      ((:regex ".*" :case-sensitive? t) :ok))

  ;; Expected matching results
  t t t t)
