;;;; composite-filter.lisp --- Unit test for composite filter classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

;;; `complement-filter'

(deftestsuite complement-filter-root (filter-root
                                      filter-suite)
  ((simple-filter (filter '(:not (:scope :scope "/foo")))))
  (:documentation
   "Unit tests for the `complement-filter' class."))

(define-basic-filter-test-cases (complement-filter :complement)
    ;; construct cases
    `(;; exactly one child is required.
      (()                                                   error)
      ((:children ())                                       error)
      ((:children (,#1=(filter :scope :scope "/bar") ,#1#)) error)

      ;; these are ok
      ((:children (,(filter :scope :scope "/bar")))         t))

  ;; expected matching results
  t nil nil nil)

;;; `conjoin-filter'

(deftestsuite conjoin-filter-root (filter-root
                                   filter-suite)
  ((simple-filter (filter '(:and (:type :type string)
                                 (:scope :scope "/foo")))))
  (:documentation
   "Unit tests for the `conjoin-filter' class."))

(define-basic-filter-test-cases (conjoin-filter :and)
    ;; construct cases
    '(;; these are ok
      (()             t)
      ((:children ()) t))

  ;; expected matching results
  nil t nil nil)

;;; `disjoin-filter'

(deftestsuite disjoin-filter-root (filter-root
                                   filter-suite)
  ((simple-filter (filter '(:or (:scope :scope "/foo/bar")
                                (:type :type string)))))
  (:documentation
   "Unit tests for the `disjoin-filter' class."))

(define-basic-filter-test-cases (disjoin-filter :or)
    ;; Construction
    '(;; these are ok
      (()             t)
      ((:children ()) t))

  ;; Matching results
  t t t nil)
