;;; package.lisp --- Package definition for unit tests of the transport module.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :cl-user)

(defpackage :rsb.transport.test
  (:use
   :cl
   :alexandria
   :iterate
   :bind
   :lift

   :rsb
   :rsb.event-processing
   :rsb.transport

   :rsb.test)

  (:export
   :transport-root)

  (:export
   :connector-suite
   :check-connector-class
   :check-connector

   :define-basic-connector-test-cases)

  (:documentation
   "This package contains unit tests for the transport module."))

(in-package :rsb.transport.test)

(deftestsuite transport-root (root)
  ()
  (:documentation
   "Root unit test suite for the transport module."))

(deftestsuite connector-suite ()
  ()
  (:function
   (check-connector-class (class
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
		    :test #'equal)
       ;; Check schemas.
       (iter (for schema in schemas)
	     (ensure (typep schema 'keyword)))
       (ensure-same schemas expected-schemas
		    :test #'equal)
       ;; Check options.
       (iter (for option in options)
	     (ensure (typep option 'list))))))
  (:function
   (check-connector (connector expected-direction expected-wire-type)
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
		    :test #'string=))))
  (:documentation
   "This test suite class is intended to be used as a superclass of
transport test suites."))

(defmacro define-basic-connector-test-cases
    (class
     &key
     (suite-name        (symbolicate class "-ROOT"))
     (name              (guess-connector-name class))
     construct-args
     expected-direction
     expected-wire-type
     expected-schemas)
  "Define basic test cases for the connector class CLASS."
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

       (check-connector-class
	(find-class ',class)
	,expected-direction
	,expected-wire-type
	,expected-schemas))

     (addtest (,suite-name
	       :documentation
	       ,(format nil "Test basic properties of a ~A connector ~
instance."
			class))
       construct

       (let ((instance (make-instance ',class ,@construct-args)))
	 (check-connector
	  instance ,expected-direction ,expected-wire-type)))

     (addtest (,suite-name
          :documentation
	  ,(format nil "Test printing a ~A connector instance."
		   class))
       print
       (let ((instance (make-instance ',class ,@construct-args)))
	 (with-output-to-string (stream)
	   (print-object instance stream))))))


;;; Utility functions
;;

(defun guess-connector-name (class-name)
  "Guess the keyword naming the connector class named CLASS-NAME."
  (let* ((package-name (package-name (symbol-package class-name)))
	 (.-position   (position #\. package-name :from-end t)))
    (make-keyword (subseq package-name (1+ .-position)))))
