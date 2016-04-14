;;;; method-filter.lisp --- Unit tests for the method-filter class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite method-filter-root (filter-root)
  ()
  (:documentation
   "Test suite for the `method-filter' class."))

(define-basic-filter-tests (method-filter :method)
  '(;; missing :method initarg
    (()                  error)

    ;; these are ok
    ((:method nil)       t)
    ((:method :method)   t)
    ((:method :|method|) t)))

(define-filter-match-test (method-filter :method)
  `(((:method :foo) ("/" 1)              nil)
    ((:method :foo) ("/" 1 :method :bar) nil)
    ((:method :foo) ("/" 1 :method :foo) t)))
