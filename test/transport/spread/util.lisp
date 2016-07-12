;;;; util.lisp --- Unit tests for utilities used in the spread backend.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

(deftestsuite util-root (transport-spread-root)
  ()
  (:documentation
   "Test suite for the utility functions used in the spread
backend."))

(addtest (util-root
          :documentation
          "Smoke test for the `normalize-daemon-endpoint' function.")
  normalize-daemon-endpoint/smoke

  (ensure-cases (name host port
                 expected-name &optional expected-host expected-port)
      '((nil         nil    nil  missing-required-argument)
        ("4803@host" nil    nil  "4803@host" "host" 4803)
        (nil         "host" 4803 "4803@host" "host" 4803)
        (nil         nil    4803 "4803"      nil    4803))
    (case expected-name
      (missing-required-argument
       (ensure-condition missing-required-argument
         (normalize-daemon-endpoint name host port)))
      (t
       (ensure-same (normalize-daemon-endpoint name host port)
                    (values expected-name expected-host expected-port)
                    :test #'equal)))))

(addtest (util-root
          :documentation
          "Smoke test for the `scope->group' function.")
  scope->group/smoke

  (ensure-cases (string expected)
      '(("/"         "6666cd76f96956469e7be39d750cc7d")
        ("/foo/"     "4f87be8f6e593d167f5fd1ab238cfc2")
        ("/foo/bar/" "1c184f3891344400380281315d9e738"))
    (let ((result (scope->group (make-scope string))))
      (ensure-same result (concatenate 'string expected '(#\Null))
                   :test #'string=))))

(addtest (util-root
          :documentation
          "Smoke test for the non-caching variant of the
`scope->groups' function.")
  scope->groups/no-cache/smoke

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

(addtest (util-root
          :documentation
          "Smoke test for the `scope->groups' function.")
  scope->groups/smoke

  (let ((*scope->groups-cache* (make-scope->groups-cache)))
    (ensure-cases (scope) '("/" "/foo" "/foo/bar")
      (let ((result-1 (scope->groups (make-scope scope :intern? t)))
            (result-2 (scope->groups (make-scope scope :intern? t)))
            (result-3 (scope->groups (make-scope scope))))
        (ensure-same result-1 result-2 :test #'eq)
        (ensure-same result-1 result-3 :test #'equal)))))

(addtest (util-root
          :documentation
          "Test the cache flushing mechanism of the cache used by
`scope->groups'.")
  scope->groups/cache-flush

  (let ((*scope->groups-cache* (make-scope->groups-cache)))
    (iter (repeat (* 10 *scope->groups-cache-max-size*))
          (ensure (<= (hash-table-count *scope->groups-cache*)
                      *scope->groups-cache-max-size*))
          (scope->groups (make-scope "/bla/bli/boo"))
          (ensure (<= (hash-table-count *scope->groups-cache*)
                      *scope->groups-cache-max-size*)))))
