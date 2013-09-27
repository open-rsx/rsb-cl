;;;; error-policy-mixin.lisp --- Unit tests for the error-policy-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.event-processing.test)

(deftestsuite error-policy-mixin-root (event-processing-root)
  ((simple-processor (make-instance 'error-policy-mixin)))
  (:documentation
   "Test suite for the `error-policy-mixin' class."))

(defun signaling-function ()
  "A function that unconditionally signals an error."
  (restart-case
      (error "~@<I like to signal.~@:>")
    (log      (condition) (declare (ignore condition)) nil)
    (continue () nil)))

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

	  ;; The error policy #'continue should prevent the error from
	  ;; being signaled.
	  (setf (processor-error-policy simple-processor) #'continue)
	  ,@invoke-form

	  ;; The error policy #'log-error should prevent the error
	  ;; from being signaled.
	  (setf (processor-error-policy simple-processor) #'log-error)
	  ,@invoke-form)))

  (define-smoke-test smoke/function
      (invoke-with-error-policy simple-processor #'signaling-function))

  (define-smoke-test smoke/macro
      (with-error-policy (simple-processor) (signaling-function))))
