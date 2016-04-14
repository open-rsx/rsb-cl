;;;; composite-filter.lisp --- Unit test for composite filter classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

;;; `complement-filter'

(deftestsuite complement-filter-root (filter-root)
  ()
  (:documentation
   "Unit tests for the `complement-filter' class."))

(define-basic-filter-tests (complement-filter :complement)
  `(;; exactly one child is required.
    (()                                                   error)
    ((:children ())                                       error)
    ((:children (,#1=(filter :scope :scope "/bar") ,#1#)) error)

    ;; these are ok
    ((:children (,(filter :scope :scope "/bar")))         t)))

(define-filter-match-test (complement-filter :complement)
  '((((:type :type integer)) ("/" "bar")    t)
    (((:type :type integer)) ("/" 1)        nil)))

;;; `conjoin-filter'

(deftestsuite conjoin-filter-root (filter-root)
  ()
  (:documentation
   "Unit tests for the `conjoin-filter' class."))

(define-basic-filter-tests (conjoin-filter :and)
  '(;; these are ok
    (()             t)
    ((:children ()) t)))

(define-filter-match-test (conjoin-filter :and)
  '((()                                            ("/"        "bar")    t)

    (((:type :type string))                        ("/"        "bar")    t)
    (((:type :type string))                        ("/"        1)        nil)

    (((:type :type string) (:scope :scope "/foo")) ("/"        "bar")    nil)
    (((:type :type string) (:scope :scope "/foo")) ("/"        1)        nil)
    (((:type :type string) (:scope :scope "/foo")) ("/foo/bar" "baz")    t)
    (((:type :type string) (:scope :scope "/foo")) ("/foo/bar" 1)        nil)))

;;; `disjoin-filter'

(deftestsuite disjoin-filter-root (filter-root)
  ()
  (:documentation
   "Unit tests for the `disjoin-filter' class."))

(define-basic-filter-tests (disjoin-filter :or)
  '(;; these are ok
    (()             t)
    ((:children ()) t)))

(define-filter-match-test (disjoin-filter :or)
  '((()                                            ("/"        "bar")    nil)

    (((:type :type string))                        ("/"        "bar")    t)
    (((:type :type string))                        ("/"        1)        nil)

    (((:type :type string) (:scope :scope "/foo")) ("/"        "bar")    t)
    (((:type :type string) (:scope :scope "/foo")) ("/"        1)        nil)
    (((:type :type string) (:scope :scope "/foo")) ("/foo/bar" "baz")    t)
    (((:type :type string) (:scope :scope "/foo")) ("/foo/bar" 1)        t)))
