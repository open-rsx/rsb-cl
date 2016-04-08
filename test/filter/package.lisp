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

   #:fiveam

   #:rsb
   #:rsb.filter

   #:rsb.test)

  (:export
   #:define-basic-filter-tests
   #:define-filter-match-test)

  (:documentation
   "This package contains unit tests for the filter module."))

(cl:in-package #:rsb.filter.test)

(def-suite filter-root
  :in root
  :description
  "Root unit test suite for the filter module.")

;;; Utilities

(defun call-with-filter-checking-thunk
    (thunk filter event-spec)
  "Call THUNK with a function as the sole argument, that
   1. Constructs an `access-checking-event' according to EVENT-SPEC
   2. Applies FILTER rule to the event
   3. Returns the matching result"
  (let+ (((&flet check (funcall?)
            (let+ ((event (apply #'make-access-checking-event-for-processor
                                 filter event-spec))
                   ((&flet do-it ()
                      (with-access-checking ()
                        (if funcall?
                            (funcall filter event)
                            (matches? filter event))))))
              (funcall thunk #'do-it)))))
    (check nil)
    (check t)))

(defmacro define-basic-filter-tests ((class spec) construct-cases)
  "Define basic test cases for the filter class CLASS."
  `(progn
     (test construct
       ,(format nil "Test construction instances of the `~(~A~)' ~
                     filter class."
                class)

       (mapc (lambda+ ((args expected))
               (case expected
                 (error
                  (signals error (apply #'make-instance ',class args)))
                 (t
                  (apply #'make-instance ',class args)
                  (apply #'make-filter ,spec args)
                  (apply #'filter ,spec args))))
             ,construct-cases))

     (test print
       ,(format nil "Test method on `print-object' for the `~(~A~)' ~
                     filter class."
                class)

       (mapc (lambda+ ((initargs &ign))
               (check-print (apply #'make-filter ,spec initargs)))
             (remove t ,construct-cases :key #'second :test-not #'eq)))))

(defmacro define-filter-match-test ((class spec) cases)
  "Define a test case for the `matches?' method of filter class
   CLASS."
  `(test smoke
     ,(format nil "Smoke test for the `~(~A~)' filter class." class)

     (mapc
      (lambda+ ((filter-initargs event-spec expected))
        (let ((filter (apply #'filter (if (listp (first filter-initargs))
                                          (list (list* ,spec filter-initargs))
                                          (list* ,spec filter-initargs)))))
          (call-with-filter-checking-thunk
           (lambda (do-it)
             (let ((result (funcall do-it)))
               (is (eql expected result)
                   "~@<The filter ~S ~:[did not match~;matched~] the ~
                    event ~S, but should~:[ not~;~].~@:>"
                   filter result event-spec expected)))
           filter
           event-spec)))
      ,cases)))
