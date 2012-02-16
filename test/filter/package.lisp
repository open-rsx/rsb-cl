;;; package.lisp --- Package definition for unit tests of the filter module.
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
		 (make-event "/foo/bar" 1)
		 (make-event "/foo"     1))))
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
