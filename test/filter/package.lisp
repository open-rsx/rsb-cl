;;;; package.lisp --- Package definition for unit tests of the filter module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.filter.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:rsb
   #:rsb.filter

   #:rsb.test)

  (:export
   #:define-basic-filter-test-cases)

  (:documentation
   "This package contains unit tests for the filter module."))

(cl:in-package #:rsb.filter.test)

(deftestsuite filter-root (root)
  ()
  (:documentation
   "Root unit test suite for the filter module."))

(deftestsuite filter-suite ()
  ((events (list (make-event "/"        "bar")
                 (make-event "/foo/bar" "baz")
                 (make-event "/foo/bar" 1)
                 (make-event "/foo"     1 :foo 1))))
  (:documentation
   "This class can be mixed into test suite classes which contain
tests for filters."))

;;; Utilities

(defun call-with-filter-checking-thunk
    (thunk filter-spec event-spec)
  "Call THUNK with a function as the sole argument, that
   1. Constructs a filter according to FILTER-SPEC
   2. Constructs an `access-checking-event' according to EVENT-SPEC
   3. Applies the filter rule to the event
   4. Returns the filter result"
  (let+ (((&flet check (funcall?)
            (let+ ((filter filter-spec #+later (apply #'make-filter
                                                      (ensure-list filter-spec)))
                   (event  (apply #'make-access-checking-event-for-processor
                                  filter event-spec))
                   ((&flet do-it ()
                      (with-access-checking ()
                        (if funcall?
                            (funcall filter event)
                            (matches? filter event))))))
              (funcall thunk #'do-it)))))
    (check nil)
    (check t)))

(defmacro define-basic-filter-test-cases ((class spec) construct-cases &rest matches)
  "Define basic test cases for the filter class CLASS."
  (let ((suite-name (symbolicate class "-ROOT")))
    `(progn
       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test construction instances of the ~
                               `~(~A~)' filter class."
                          class))
         construct

         (ensure-cases (args expected)
             ,construct-cases

           (case expected
             (error
              (ensure-condition error
                (apply #'make-instance ',class args)))
             (t
              (apply #'make-instance ',class args)
              (apply #'make-filter ,spec args)
              (apply #'filter ,spec args)))))

       (addtest (,suite-name
                 :documentation
                 ,(format nil "Smoke test for the `~(~A~)' filter class."
                          class))
         smoke

         (ensure-cases (event expected)
             (map 'list #'list events ',matches)
           (call-with-filter-checking-thunk
            (lambda (do-it)
              (let ((result (funcall do-it)))
                (ensure-same result expected
                             :report    "~@<The filter ~S ~:[did not ~
                                         match~;matched~] the event ~S, ~
                                         but should~:[ not~;~].~@:>"
                             :arguments (simple-filter result event expected))))
            simple-filter (list* (event-scope event) (event-data event)
                                 :method (event-method event)
                                 (meta-data-plist event))))) ; TODO event-spec

       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test method on `print-object' for the ~
                               `~(~A~)' filter class."
                          class))
         print

         (check-print simple-filter)))))
