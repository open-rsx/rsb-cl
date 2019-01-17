;;;; scope-filter.lisp --- Unit tests for the scope-filter class.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(def-suite* scope-filter-root
  :in filter-root
  :description
  "Unit tests for the `scope-filter' class.")

(define-basic-filter-tests (scope-filter :scope)
  '(;; Some invalid cases.
    (()                          error) ; missing required initargs
    ((:scope "<>!")              error) ; invalid scope

    ;; These are ok.
    ((:scope "/foo")             t)
    ((:scope "/foo" :exact? nil) t)
    ((:scope "/foo" :exact? t)   t)))

(define-filter-match-test (scope-filter :scope)
  '(((:scope "/foo")          ("/"        1)    nil)
    ((:scope "/foo")          ("/foo"     1)    t)
    ((:scope "/foo")          ("/foo/bar" 1)    t)
    ((:scope "/foo":exact? t) ("/"        1)    nil)
    ((:scope "/foo":exact? t) ("/foo"     1)    t)
    ((:scope "/foo":exact? t) ("/foo/bar" 1)    nil)))
