;;;; conditions.lisp --- Conditions used in the patterns.request-reply module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

(define-condition no-such-method (no-such-child-error)
  ()
  (:default-initargs
   :name (missing-required-initarg 'no-such-method :name))
  (:report
   (lambda (condition stream)
     (format stream "~@<The specified method ~S does not exist.~@:>"
             (second (child-condition-key condition)))))
  (:documentation
   "This error is signaled when a specified method does not exist."))

(defun no-such-method-name (condition)
  (second (child-condition-key condition)))

(define-condition remote-call-error (rsb-error
                                     chainable-condition)
  ((method  :initarg  :method
            :type     method
            :reader   remote-call-error-method
            :documentation
            "Stores the method of the failed call.")
   (request :initarg  :request
            :reader   remote-call-error-request
            :documentation
            "Stores the request object that was passed to the method
in the failed call."))
  (:default-initargs
   :method  (missing-required-initarg 'remote-call-error :method)
   :request (missing-required-initarg 'remote-call-error :request))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to call method ~A with request ~
                     ~A~/more-conditions:maybe-print-cause/~:@>"
             (remote-call-error-method  condition)
             (remote-call-error-request condition)
             condition)))
  (:documentation
   "This error is signaled when a remote method call fails for some
    reason."))

(define-condition remote-method-execution-error (remote-call-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Remote method ~A failed to execute for ~
                     request ~
                     ~A~/more-conditions:maybe-print-cause/~:@>"
             (remote-call-error-method  condition)
             (remote-call-error-request condition)
             condition)))
  (:documentation
   "This error is signaled when a call to a remote method succeeds in
    calling the method on the remote side but fails in the actual
    remote method."))
