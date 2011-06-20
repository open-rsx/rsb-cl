;;; error-policy-mixin.lisp --- Unit tests for the error-policy-mixin class.
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

(in-package :rsb.event-processing.test)

(deftestsuite error-policy-mixin-root (event-processing-root)
  ((simple-processor (make-instance 'error-policy-mixin)))
  (:documentation
   "Test suite for the `error-policy-mixin' class."))

(defun signaling-function ()
  "A function that unconditionally signals an error."
  (restart-case
      (error "~@<I like to signal.~@:>")
    (log    (condition) (declare (ignore condition)) nil)
    (ignore () nil)))

(macrolet
    ((define-smoke-test (name &body invoke-form)
       `(addtest (error-policy-mixin-root
		  :documentation
		  "Test basic error handling policies of the
`error-policy-mixin' class.")
	  ,name

	  ;; Error policy nil means to just unwind.
	  (setf (processor-error-policy simple-processor) nil)
	  (ensure-condition 'simple-error
	    ,@invoke-form)

	  ;; The error policy #'ignore-error should prevent the error
	  ;; from being signaled.
	  (setf (processor-error-policy simple-processor) #'ignore-error)
	  ,@invoke-form

	  ;; The error policy #'log-error should prevent the error
	  ;; from being signaled.
	  (setf (processor-error-policy simple-processor) #'log-error)
	  ,@invoke-form)))

  (define-smoke-test smoke/function
      (invoke-with-error-policy simple-processor #'signaling-function))

  (define-smoke-test smoke/macro
      (with-error-policy (simple-processor) (signaling-function))))
