;;; package.lisp --- Package definition for unit tests of the filter module.
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

(cl:defpackage :rsb.filter.test
  (:use
   :cl
   :alexandria
   :lift

   :rsb
   :rsb.filter

   :rsb.test)

  (:export
   :define-basic-filter-test-cases)

  (:documentation
   "This package contains unit tests for the filter module"))

(cl:in-package :rsb.filter.test)

(deftestsuite filter-root (root)
  ()
  (:documentation
   "Root unit test suite for the filter module."))

(deftestsuite filter-suite ()
  ((events (list (make-event "/"        "bar")
		 (make-event "/foo/bar" "baz")
		 (make-event "/foo/bar" "baz")
		 (make-event/typed "/foo/bar" "1" 'string)
		 (make-event/typed "/foo/bar" 1   'integer)
		 (make-event/typed "/foo/bar" 1   'fixnum)
		 (make-event/typed "/foo"     1   'fixnum))))
  (:documentation
   "This class can be mixed into test suite classes which contain
tests for filters."))

(defmacro define-basic-filter-test-cases ((class spec) construct-cases &rest matches)
  "Define basic test cases for the filter class CLASS."
  (let ((suite-name (symbolicate class "-ROOT")))
    `(progn
       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test construction instances of the `~(~A~)' filter class."
			  class))
	 construct

	 (ensure-cases (args expected)
	     ,construct-cases

	   (if (eq expected :error)
	       (ensure-condition error
		 (apply #'make-instance ',class args))
	       (progn
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
	   (let ((result (matches? simple-filter event)))
	     (ensure-same result expected
			  :report    "~@<The filter ~S ~:[did not ~
match~;matched~] the event ~S, but should~:[ not~;~].~@:>"
			  :arguments (simple-filter result event expected)))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test calling instances of the `~(~A~)' as functions."
			  class))
	 funcallability

	 (ensure-cases (event expected)
	     (map 'list #'list events ',matches)
	   (let ((result (funcall simple-filter event)))
	     (ensure-same result expected
			  :report "~@<When called as a function, the ~
filter ~S ~:[did not match~;matched~] the event ~S, but should~:[ ~
not~;~].~@:>"
			  :arguments (simple-filter result event expected)))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test method on `print-object' for the `~(~A~)' filter class."
			  class))
	 print

	 (check-print simple-filter)))))
