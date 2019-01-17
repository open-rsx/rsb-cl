;;;; package.lisp --- Package definition for unit tests of the transport module.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus

   #:fiveam

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

(def-suite transport-root
  :in root
  :description
  "Root unit test suite for the transport module.")

;;; Test utilities

(defun check-connector-class (class
                              expected-schemas
                              expected-wire-type
                              expected-remote?
                              expected-direction)
  (let ((schemas   (transport-schemas class))
        (wire-type (transport-wire-type class))
        (remote?   (transport-remote? class))
        (direction (connector-direction class))
        (options   (connector-options class)))
    ;; Check schemas.
    (iter (for schema in schemas)
          (is (typep schema 'keyword)))
    (is (set-equal expected-schemas schemas))
    ;; Check wire-type.
    (is (typep wire-type 'wire-type))
    (is (type= expected-wire-type wire-type))
    ;; Check remote.
    (is (eq expected-remote? remote?))
    ;; Check direction.
    (is (typep direction 'direction))
    (is (eq expected-direction direction))
    ;; Check options.
    (iter (for option in options)
          (is (typep option 'list)))))

(defun check-connector (connector
                        expected-schemas
                        expected-wire-type
                        expected-remote?
                        expected-direction )
  (let ((schemas   (transport-schemas      connector))
        (wire-type (transport-wire-type    connector))
        (remote?   (transport-remote?      connector))
        (direction (connector-direction    connector))
        (url       (connector-url          connector))
        (rel-url   (connector-relative-url connector "/foo")))
    ;; Check schemas.
    (iter (for schema in schemas)
          (is (typep schema 'keyword)))
    (is (set-equal expected-schemas schemas))
    ;; Check wire-type.
    (is (typep wire-type 'wire-type))
    (is (type= expected-wire-type wire-type))
    ;; Check remote.
    (is (eq expected-remote? remote?))
    ;; Check direction.
    (is (typep direction 'direction))
    (is (eq expected-direction direction))
    ;; Check URLs.
    (is (typep url 'puri:uri))
    (is (typep rel-url 'puri:uri))
    (is (eq (first expected-schemas) (puri:uri-scheme url)))
    (is (string= "/foo/" (puri:uri-path rel-url)))))

(defmacro define-basic-connector-test-cases
    (class
     &key
     (name              (%guess-connector-name class))
     suite
     initargs
     expected-schemas
     expected-wire-type
     expected-remote?
     expected-direction)
  "Define basic test cases for the connector class CLASS.

   NAME is a keyword designating TRANSPORT e.g. in calls to
   `rsb.transport:make-connector'.

   SUITE is a symbol naming the test suite part of which the generated
   test cases should be.

   INITARGS are initargs which should be used when making connector
   instances.

   EXPECTED-SCHEMAS specifies the list of expected schemas of the
   connector class.

   EXPECTED-WIRE-TYPE specifies the expected wire-type of the
   connector class.

   EXPECTED-REMOTE? specifies whether CLASS is expected to belong to a
   remote transport.

   EXPECTED-DIRECTION specifies the expected direction of the
   connector class. "
  (flet ((test-name (name)
           (symbolicate class '#:/ name)))
    `(progn
       (test (,(test-name '#:find-connector-class)
              ,@(when suite `(:suite ,suite)))
         ,(format nil "Test whether `find-connector-class' can find ~
                       the ~A connector class."
                  class)

         (let* ((transport (service-provider:find-provider
                            'rsb.transport::transport ',name))
                (provider  (service-provider:find-provider
                            transport ,expected-direction))
                (class     (service-provider:provider-class provider)))
           (is (eq class (find-class ',class)))))

       (test (,(test-name '#:class)
              ,@(when suite `(:suite ,suite)))
         ,(format nil "Test basic properties of the ~A connector ~
                       class."
                  class)

         (check-connector-class (find-class ',class)
                                ,expected-schemas
                                ,expected-wire-type
                                ,expected-remote?
                                ,expected-direction))

       (test (,(test-name '#:construct)
              ,@(when suite `(:suite ,suite)))
         ,(format nil "Test basic properties of a ~A connector ~
                       instance."
                  class)

         (let ((instance (apply #'make-instance ',class ,initargs)))
           (check-connector instance
                            ,expected-schemas
                            ,expected-wire-type
                            ,expected-remote?
                            ,expected-direction)))

       (test (,(test-name '#:print)
              ,@(when suite `(:suite ,suite)))
         ,(format nil "Test printing a ~A connector instance."
                  class)

         (let ((instance (apply #'make-instance ',class ,initargs)))
           (is (not (emptyp (with-output-to-string (stream)
                              (print-object instance stream))))))))))

;;; Utility functions

(defun %guess-connector-name (class-name)
  "Guess the keyword naming the connector class named CLASS-NAME."
  (let* ((package-name (package-name (symbol-package class-name)))
         (.-position   (position #\. package-name :from-end t)))
    (make-keyword (subseq package-name (1+ .-position)))))
