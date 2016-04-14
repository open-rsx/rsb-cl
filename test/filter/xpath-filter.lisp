;;;; xpath-filter.lisp --- Unit tests for the xpath-filter class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite xpath-filter-root (filter-root)
  ()
  (:documentation
   "Unit tests for the `xpath-filter' class."))

(define-basic-filter-tests (xpath-filter :xpath)
  '(;; Some invalid cases.
    (()                          error) ; missing initarg
    ((:xpath 5)                  error) ; wrong type for XPath
    ((:xpath ".*")               error) ; invalid XPath

    ;; These are ok.
    ((:xpath "*")                t)
    ((:xpath "*" :always :match) t)))

(define-filter-match-test (xpath-filter :xpath)
  '(((:xpath "*")                       ("/" "bar") t)
    ((:xpath "*" :always :do-not-match) ("/" "baz") nil)))
