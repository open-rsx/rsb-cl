;;;; regex-filter.lisp --- Unit tests for the regex-filter class.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(def-suite* regex-filter-root
  :in filter-root
  :description
  "Unit tests for the `regex-filter' class.")

(define-basic-filter-tests (regex-filter :regex)
  '(;; Some invalid cases.
    (()                               error) ; missing initarg
    ((:regex 5)                       error) ; wrong type for regex
    ((:regex "*")                     error) ; invalid regex

    ;; These are ok.
    ((:regex ".*")                    t)
    ((:regex ".*" :always :match)     t)
    ((:regex ".*" :case-sensitive? t) t)))

(define-filter-match-test (regex-filter :regex)
  '(((:regex "^bar$")                      ("/" "bar") t)
    ((:regex "^bar$")                      ("/" "BAR") nil)
    ((:regex "^bar$")                      ("/" "baz") nil)
    ((:regex "^bar$")                      ("/" "BAZ") nil)

    ((:regex "^bar$" :case-sensitive? nil) ("/" "bar") t)
    ((:regex "^bar$" :case-sensitive? nil) ("/" "BAR") t)
    ((:regex "^bar$" :case-sensitive? nil) ("/" "baz") nil)
    ((:regex "^bar$" :case-sensitive? nil) ("/" "BAZ") nil)

    ((:regex ".*")                         ("/" 1)     t)
    ((:regex ".*" :always :do-not-match)   ("/" 1)     nil)))
