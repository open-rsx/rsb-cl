;;; package.lisp --- Package definition cl-rsb unit tests.
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

(in-package :cl-user)

(defpackage :rsb.test
  (:use
   :cl
   :alexandria
   :bind
   :iterate

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

   :participant-suite)

  (:documentation
   "This package contains unit tests for the cl-rsb system."))

(in-package :rsb.test)

(deftestsuite root ()
  ((spread-port (asdf:component-property
		 (asdf:find-system :cl-rsb-test) :spread-port)))
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
      (typep (event-id event) 'uuid:uuid))
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

(defmacro define-basic-participant-test-cases (kind)
  "Define basic test cases for the participant subclass designated by
KIND."
  (let ((suite-name (symbolicate kind "-ROOT"))
	(with-name  (symbolicate "WITH-" kind)))
    `(progn
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
