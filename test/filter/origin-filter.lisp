;;;; origin-filter.lisp --- Unit tests for origin filter.
;;;;
;;;; Copyright (C) 2012, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(def-suite origin-filter-root
  :in filter-root
  :description
  "Test suite for the `origin-filter' class.")
(in-suite origin-filter-root)

(define-basic-filter-tests (origin-filter :origin)
  `(;; Some invalid cases.
    (()                                                         error) ; missing initarg
    ((:origin 5)                                                error) ; wrong origin for origin specifier
    ((:origin "00000000-0000-0000-0000-")                       error) ; UUID syntax error

    ;; These are ok.
    ((:origin ,(uuid:make-null-uuid))                           t)
    ((:origin "00000000-0000-0000-0000-000000000000")           t)
    ((:origin ,(uuid:uuid-to-byte-array (uuid:make-null-uuid))) t)))

(define-filter-match-test (origin-filter :origin)
  `(;; Events without UUID
    ((:origin ,(uuid:make-null-uuid)) ("/" nil) nil)
    ((:origin ,(uuid:make-v4-uuid))   ("/" nil) nil)
    ;; Events with UUID
    ((:origin ,(uuid:make-null-uuid))
     ("/" nil :origin ,(uuid:make-null-uuid))
     t)
    ((:origin ,(uuid:make-null-uuid))
     ("/" nil :origin ,(uuid:make-v4-uuid))
     nil)
    ((:origin ,(uuid:make-v4-uuid))
     ("/" nil :origin ,(uuid:make-null-uuid))
     nil)))
