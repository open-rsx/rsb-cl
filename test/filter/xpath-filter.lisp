;;;; xpath-filter.lisp --- Unit tests for the xpath-filter class.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(deftestsuite xpath-filter-root (filter-root)
  ()
  (:documentation
   "Unit tests for the `xpath-filter' class."))

(define-basic-filter-tests (xpath-filter :xpath)
  '(;; Some invalid cases.
    (()                                             error) ; missing initarg
    ((:xpath 5)                                     error) ; wrong type for XPath
    ((:xpath ".*")                                  error) ; invalid XPath

    ;; These are ok.
    ((:xpath "*")                                   t)
    ((:xpath "*" :namespaces (("" . "http://foo"))) t)
    ((:xpath "*" :builder t)                        t)))

(define-filter-match-test (xpath-filter :xpath)
  `(((:xpath "@no-such-attribute")              ("/"    "bar")                     nil)
    ((:xpath "no-such-node")                    ("/"    "bar")                     nil)
    ((:xpath "*")                               ("/"    "bar")                     t)
    ((:xpath ".//*")                            ("/"    "bar")                     t)

    ((:xpath "@scope")                          ("/foo" "bar")                     t)
    ((:xpath "contains(@scope, \"foo\")")       ("/foo" "bar")                     t)
    ((:xpath "contains(@scope, \"bar\")")       ("/foo" "bar")                     nil)

    ((:xpath "not(@origin)")                    ("/"    "bar")                     t)
    ((:xpath "@origin")
     ("/" "bar" :origin ,(uuid:make-null-uuid))
     t)

    ((:xpath "not(@sequence-number)")           ("/"    "bar")                     t)
    ((:xpath "@sequence-number > 0")            ("/"    "bar" :sequence-number 10) t)

    ((:xpath "not(@method)")                    ("/"    "bar")                     t)
    ((:xpath "@method")                         ("/"    "bar" :method :|foo|)      t)
    ((:xpath "@method = 'foo'")                 ("/"    "bar" :method :|foo|)      t)

    ((:xpath "data/node()")                     ("/"    "bar")                     t)
    ((:xpath "data/node() = 'bar'")             ("/"    "bar")                     t)

    ((:xpath "meta-data/node()")                ("/"    "bar")                     nil)
    ((:xpath "meta-data[@key='foo']/node()")    ("/"    "bar" :|foo| "a")          t)

    ((:xpath "timestamp[@key='CREATE']/node()") ("/"    "bar")                     t)

    ((:xpath "cause/node()")                    ("/"    "bar")                     nil)
    ((:xpath "cause/node()")
     ("/" "bar" :causes (,(cons (uuid:make-null-uuid) 0)))
     t)

    ;; Some functions and operators.
    ((:xpath "count(cause) = 0")                ("/"    "bar")                     t)
    ((:xpath "contains(data/node(), 'ar')")     ("/"    "bar")                     t)
    ((:xpath "contains(data/node(), 'az')")     ("/"    "bar")                     nil)
    ((:xpath "data/node() = 0")                 ("/"    0)                         t)))
