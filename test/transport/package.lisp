;;; package.lisp --- Package definition for unit tests of the transport module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:defpackage :rsb.transport.test
  (:use
   :cl
   :alexandria
   :iterate
   :let-plus
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

(cl:in-package :rsb.transport.test)

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
		    :test #'type=)
       ;; Check schemas.
       (iter (for schema in schemas)
	     (ensure (typep schema 'keyword)))
       (ensure-same schemas expected-schemas
		    :test #'set-equal)
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
     (name              (%guess-connector-name class))
     (suite-name        (symbolicate class "-ROOT"))
     initargs
     expected-direction
     expected-wire-type
     expected-schemas)
  "Define basic test cases for the connector class CLASS.

NAME is a keyword designating TRANSPORT e.g. in calls to
`rsb.transport:make-transport'.

SUITE-NAME is a symbol naming the test suite part of which the
generated test cases should be.

INITARGS are initargs which should be used when making connector
instances.

EXPECTED-DIRECTION specifies the expected direction of the connector
class.

EXPECTED-WIRE-TYPE specifies the expected wire-type of the connector
class.

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
;;

(defun %guess-connector-name (class-name)
  "Guess the keyword naming the connector class named CLASS-NAME."
  (let* ((package-name (package-name (symbol-package class-name)))
	 (.-position   (position #\. package-name :from-end t)))
    (make-keyword (subseq package-name (1+ .-position)))))
