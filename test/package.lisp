;;; package.lisp --- Package definition cl-rsb unit tests.
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

(cl:defpackage :rsb.test
  (:use
   :cl
   :alexandria
   :let-plus
   :iterate
   :more-conditions

   :lift

   :rsb
   :rsb.filter)

  ;; Root test suite
  (:export
   :root)

  ;; Test environment
  (:export
   :spread-port)

  ;; Test utilities
  (:export
   :check-print

   :check-event

   :participant-suite
   :define-basic-participant-test-cases

   :define-restart-method-test-case)

  (:documentation
   "This package contains unit tests for the cl-rsb system."))

(cl:in-package :rsb.test)

(deftestsuite root ()
  ((spread-port (asdf:component-property
		 (asdf:find-system :cl-rsb-test) :spread-port)))
  (:function
   (make-id (source)
     (uuid:make-uuid-from-string source)))
  (:function
   (check-print (thing)
     (ensure
      (funcall
       (conjoin #'stringp (complement #'emptyp))
       (with-output-to-string (stream)
	 (print-object thing stream))))))
  (:function
   (check-event (event scope data)
     (ensure
      (typep (event-id event) '(or null uuid:uuid)))
     (ensure-same
      (event-scope event) (make-scope scope)
      :test #'scope=)
     (ensure-same
      (event-data event) data
      :test #'equalp)))
  (:timeout 20)
  (:documentation
   "Root unit test suite of the cl-rsb system."))

(deftestsuite participant-suite ()
  ()
  (:dynamic-variables
   (*default-configuration* '(((:transport :inprocess :enabled) . "1"))))
  (:function
   (check-participant (participant scope)
     (ensure-same
      (participant-scope participant) (make-scope scope)
      :test #'scope=)
     (ensure
      (typep (participant-id participant) 'uuid:uuid))
     ;; URI stuff
     (relative-url participant)
     (abstract-uri participant)
     (let ((urls (transport-specific-urls participant)))
       (ensure (length= 1 urls)
	       :report    "~@<The participant has ~D transport-specific ~
URLs (~{~A~^, ~}), not ~D.~@:>"
	       :arguments ((length urls) urls 1)))))
  (:documentation
   "This test suite class can be used as a superclass for test suites
that test participant classes."))

(defmacro define-basic-participant-test-cases (kind &body cases)
  "Define basic test cases for the participant subclass designated by
KIND."
  (let ((suite-name (symbolicate kind "-ROOT"))
	(make-name  (symbolicate "MAKE-" kind))
	(with-name  (symbolicate "WITH-" kind)))
    `(progn
       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test constructing a ~(~A~) using `~(~A~)'."
			  kind make-name))
	 construction

	 (ensure-cases (uri args expected-scope)
	     (list ,@cases)

	   (if (eq expected-scope :error)
	       (ensure-condition error
		 (apply #',make-name uri args))
	       (let ((participant (apply #',make-name uri args)))
		 (unwind-protect
		      (check-participant participant expected-scope)
		   (detach/ignore-errors participant))))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test `print-object' method on `~(~A~)' class."
			  kind))
	 print

	 (,with-name (participant ,(format nil "/~(~A~)/print" kind)
				  ,@(when (eq kind :informer) '(t)))
	   (ensure
	    (not (emptyp
		  (with-output-to-string (stream)
		    (format stream "~A" participant))))))))))

(defmacro define-restart-method-test-case ((class method (instance-var &rest args)
				            &key
					    (restarts '(log continue)))
					   &body body)
    (let ((suite-name (symbolicate class "-ROOT"))
	  (case-name  (symbolicate method "-SMOKE"))
	  (var-name   (symbolicate "*" method "-FAIL?*")))
      `(progn
	 ;; Define a method that signals an error unless a variable is
	 ;; set.
	 (defvar ,var-name t)

	 (defmethod ,method ((,instance-var ,class) ,@args)
	   (when ,var-name (error "~@<~S failed.~@:>" ',method)))

	 ;; Define the test case that invokes the method and fails if
	 ;; the error is not handled by restarts.
	 (addtest (,suite-name
		   :documentation
		   ,(format nil "Smoke test for the :around method on ~
`~(~A~)' provided by `~(~A~)'."
			    method class))
	   ,case-name

	   (let+ (((&flet do-one (restart)
		     (setf ,var-name t)
		     (handler-bind
			 ((error #'(lambda (condition)
				     (declare (ignore condition))
				     (setf ,var-name nil)
				     (invoke-restart (find-restart restart)))))
		       ,@body))))
	     ,@(iter (for restart in restarts)
		     (collect `(do-one ',restart))))))))
