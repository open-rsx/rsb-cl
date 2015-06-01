;;;; package.lisp --- Package definition for unit tests of the transport module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:lift

   #:rsb
   #:rsb.event-processing
   #:rsb.transport

   #:rsb.test)

  (:export
   #:transport-root)

  (:export
   #:check-connector-class
   #:check-connector

   #:define-basic-connector-test-cases)

  (:documentation
   "This package contains unit tests for the transport module."))

(cl:in-package #:rsb.transport.test)

(deftestsuite transport-root (root)
  ()
  (:documentation
   "Root unit test suite for the transport module."))

;;; Test utilities

(defun check-connector-class (class
                              expected-direction
                              expected-wire-type
                              expected-schemas)
  (let ((direction (connector-direction class))
        (wire-type (connector-wire-type class))
        (schemas   (connector-schemas class))
        (options   (connector-options class)))
    ;; Check direction.
    (ensure (typep direction 'direction))
    (ensure-same direction expected-direction
                 :test #'eq)
    ;; Check wire-type
    (ensure (typep wire-type 'wire-type))
    (ensure-same wire-type expected-wire-type
                 :test #'type=)
    ;; Check schemas.
    (iter (for schema in schemas)
          (ensure (typep schema 'keyword)))
    (ensure-same schemas expected-schemas
                 :test #'set-equal)
    ;; Check options.
    (iter (for option in options)
          (ensure (typep option 'list)))))

(defun check-connector (connector expected-direction expected-wire-type)
  ;; Check direction.
  (let ((direction (connector-direction connector))
        (wire-type (connector-wire-type connector))
        (url       (connector-url connector))
        (rel-url   (connector-relative-url connector "/foo")))
    ;; Check direction.
    (ensure (typep direction 'direction))
    (ensure-same direction expected-direction
                 :test #'eq)
    ;; Check wire-type
    (ensure (typep wire-type 'wire-type))
    (ensure-same wire-type expected-wire-type
                 :test #'equal)
    ;; Check URLs.
    (ensure (typep url 'puri:uri))
    (ensure (typep rel-url 'puri:uri))
    (ensure-same (puri:uri-path rel-url) "/foo/"
                 :test #'string=)))

(defmacro define-basic-connector-test-cases
    (class
     &key
     (name              (%guess-connector-name class))
     (suite-name        (symbolicate class "-ROOT"))
     initargs
     expected-direction
     expected-wire-type
     expected-schemas)
  "Define basic test cases for the connector class CLASS.

   NAME is a keyword designating TRANSPORT e.g. in calls to
   `rsb.transport:make-connector'.

   SUITE-NAME is a symbol naming the test suite part of which the
   generated test cases should be.

   INITARGS are initargs which should be used when making connector
   instances.

   EXPECTED-DIRECTION specifies the expected direction of the
   connector class.

   EXPECTED-WIRE-TYPE specifies the expected wire-type of the
   connector class.

   EXPECTED-SCHEMAS specifies the list of expected schemas of the
   connector class."
  `(progn
     (addtest (,suite-name
          :documentation
          ,(format nil "Test whether `find-connector-class' can find ~
                        the ~A connector class."
                   class))
       find-connector-class

       (ensure-same (find-class ',class)
                    (find-connector-class ',name ,expected-direction)
                    :test #'eq))

     (addtest (,suite-name
               :documentation
               ,(format nil "Test basic properties of the ~A connector ~
                             class."
                        class))
       class

       (check-connector-class (find-class ',class)
                              ,expected-direction
                              ,expected-wire-type
                              ,expected-schemas))

     (addtest (,suite-name
               :documentation
               ,(format nil "Test basic properties of a ~A connector ~
                             instance."
                        class))
       construct

       (let ((instance (apply #'make-instance ',class ,initargs)))
         (check-connector
          instance ,expected-direction ,expected-wire-type)))

     (addtest (,suite-name
               :documentation
               ,(format nil "Test printing a ~A connector instance."
                        class))
       print

       (let ((instance (apply #'make-instance ',class ,initargs)))
         (with-output-to-string (stream)
           (print-object instance stream))))))

;;; Utility functions

(defun %guess-connector-name (class-name)
  "Guess the keyword naming the connector class named CLASS-NAME."
  (let* ((package-name (package-name (symbol-package class-name)))
         (.-position   (position #\. package-name :from-end t)))
    (make-keyword (subseq package-name (1+ .-position)))))
