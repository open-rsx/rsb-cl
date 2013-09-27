;;;; conditions.lisp --- Conditions used by the event-processing module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.event-processing)

(define-condition transform-error (error
				   chainable-condition)
  ((transform :initarg  :transform
	      :reader   transform-error-transform
	      :documentation
	      "Stores the failed transformed.")
   (object    :initarg  :object
	      :reader   transform-error-object
	      :documentation
	      "Stores the object for which the transform failed."))
  (:default-initargs
   :transform (missing-required-initarg 'transform-error :transform)
   :object    (missing-required-initarg 'transform-error :object))
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not apply transform ~A to ~
~A~/more-conditions::maybe-print-cause/~@:>"
	     (transform-error-transform condition)
	     (transform-error-object    condition)
	     condition)))
  (:documentation
   "This error is signaled when a transform fails."))
