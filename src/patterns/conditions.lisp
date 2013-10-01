;;;; conditions.lisp --- Conditions used in the patterns module of cl-rsb.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.patterns)

(define-condition no-such-method (rsb-error)
  ((name :initarg  :name
         :type     string
         :reader   no-such-method-name
         :documentation
         "Stores the name of the method that was specified but could
not be found."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The specified method ~S does not exist.~@:>"
             (no-such-method-name condition))))
  (:documentation
   "This error is signaled when a specified method does not exist."))

(define-condition remote-call-failed (rsb-error
                                      chainable-condition)
  ((method  :initarg  :method
            :type     method
            :reader   remote-call-failed-method
            :documentation
            "Stores the method of the failed call.")
   (request :initarg  :request
            :reader   remote-call-failed-request
            :documentation
            "Stores the request object that was passed to the method
in the failed call."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to call method ~A with request ~
~A~/more-conditions::maybe-print-cause/~:@>"
             (remote-call-failed-method  condition)
             (remote-call-failed-request condition)
             condition)))
  (:documentation
   "This error is signaled when a remote method call fails for some
reason."))

(define-condition remote-method-execution-error (remote-call-failed)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Remote method ~A failed to execute for ~
request ~A~/more-conditions::maybe-print-cause/~:@>"
             (remote-call-failed-method  condition)
             (remote-call-failed-request condition)
             condition)))
  (:documentation
   "Error of this class are raised when a call to a remote method
succeeds in calling the method on the remote side but fails in the
actual remote method."))
