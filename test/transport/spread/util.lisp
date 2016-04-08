;;;; util.lisp --- Unit tests for utilities used in the spread backend.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

(def-suite util-root
  :in transport-spread-root
  :description
  "Test suite for the utility functions used in the spread backend.")
(in-suite util-root)
#+TODO   (:setup
          (clrhash *scope->groups-cache*))

(test normalize-daemon-endpoint/smoke
  "Smoke test for the `normalize-daemon-endpoint' function."

  (mapc (lambda+ ((name host port
                   expected-name &optional expected-host expected-port))
         (case expected-name
           (missing-required-argument
            (signals missing-required-argument
              (normalize-daemon-endpoint name host port)))
           (t
            (ensure-same (normalize-daemon-endpoint name host port)
                         (values expected-name expected-host expected-port)
                         :test #'equal))))
                '((nil         nil    nil  missing-required-argument)
                  ("4803@host" nil    nil  "4803@host" "host" 4803)
                  (nil         "host" 4803 "4803@host" "host" 4803)
                  (nil         nil    4803 "4803"      nil    4803))))

(test scope->group/smoke
  "Smoke test for the `scope->group' function."

  (ensure-cases (string expected)
      '(("/"         "6666cd76f96956469e7be39d750cc7d")
        ("/foo/"     "4f87be8f6e593d167f5fd1ab238cfc2")
        ("/foo/bar/" "1c184f3891344400380281315d9e738"))
    (let ((result (scope->group (make-scope string))))
      (ensure-same result (concatenate 'string expected '(#\Null))
                   :test #'string=))))

(test scope->groups/no-cache/smoke
  "Smoke test for the non-caching variant of the
`scope->groups' function."

  (ensure-cases (scope expected-groups)
      '(("/"        ("6666cd76f96956469e7be39d750cc7d"))
        ("/foo"     ("6666cd76f96956469e7be39d750cc7d"
                     "4f87be8f6e593d167f5fd1ab238cfc2"))
        ("/foo/bar" ("6666cd76f96956469e7be39d750cc7d"
                     "4f87be8f6e593d167f5fd1ab238cfc2"
                     "1c184f3891344400380281315d9e738")))
    (let ((result   (scope->groups/no-cache (make-scope scope)))
          (expected (map 'list (lambda (name)
                                 (concatenate 'string name '(#\Null)))
                         expected-groups)))
      (ensure-same result expected
                   :test #'(rcurry #'set-equal :test #'string=)))))

(test scope->groups/smoke
  "Smoke test for the `scope->groups' function."

  (let ((*scope->groups-cache* (make-scope->groups-cache)))
    (ensure-cases (scope) '("/" "/foo" "/foo/bar")
      (let ((result-1 (scope->groups (make-scope scope :intern? t)))
            (result-2 (scope->groups (make-scope scope :intern? t)))
            (result-3 (scope->groups (make-scope scope))))
        (ensure-same result-1 result-2 :test #'eq)
        (ensure-same result-1 result-3 :test #'equal)))))

(test scope->groups/cache-flush
  "Test the cache flushing mechanism of the cache used by
   `scope->groups'."

  (let ((*scope->groups-cache* (make-scope->groups-cache)))
    (iter (repeat (* 10 *scope->groups-cache-max-size*))
          (ensure (<= (hash-table-count *scope->groups-cache*)
                      *scope->groups-cache-max-size*))
          (scope->groups (make-scope "/bla/bli/boo"))
          (ensure (<= (hash-table-count *scope->groups-cache*)
                      *scope->groups-cache-max-size*)))))
