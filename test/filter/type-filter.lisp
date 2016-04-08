;;;; type-filter.lisp --- Unit tests for the type-filter class.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(def-suite type-filter-root
  :in filter-root
  :description
  "Unit tests for the `type-filter' class.")
(in-suite type-filter-root)

(define-basic-filter-tests (type-filter :type)
  '(;; Some invalid cases.
    (()              error) ; missing initarg
    ((:type 5)       error) ; wrong type for type specifier

    ;; These are ok.
    ((:type t)       t)
    ((:type integer) t)))

(define-filter-match-test (type-filter :type)
  '(((:type t)       ("/" "bar") t)
    ((:type integer) ("/" "bar") nil)
    ((:type string)  ("/" "bar") t)
    ((:type integer) ("/" 5)     t)
    ((:type string)  ("/" 5)     nil)))
