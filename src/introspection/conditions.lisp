;;;; conditions.lisp --- Conditions used by the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Protocol-related conditions

(define-condition introspection-protocol-condition (rsb.patterns:protocol-condition)
  ()
  (:default-initargs
   :protocol :introspection)
  (:documentation
   "Instances of subclasses of this condition class are signaled to
    indicate occurrences of conditions related to the introspection
    protocol."))

(define-condition introspection-protocol-error (introspection-protocol-condition
                                                rsb.patterns:protocol-error)
  ()
  (:documentation
   "This error is signaled to indicate a violation of the
    introspection protocol."))

(define-condition simple-introspection-protocol-error (introspection-protocol-error
                                                       rsb.patterns:simple-protocol-condition)
  ()
  (:documentation
   "This error condition class simplifies signaling of simple
   `introspection-protocol-error' s."))

(defun introspection-error (role message cause
                            &optional format-control
                            &rest format-arguments)
  (error 'simple-introspection-protocol-error
         :role             role
         :message          message
         :cause            cause
         :format-control   format-control
         :format-arguments format-arguments))

;;; Database-related conditions

(define-condition no-such-participant-error (rsb-error)
  ((container :initarg  :container
              :reader   no-such-participant-error-container
              :documentation
              "Stores the container in which the participant could not
               be found.")
   (id        :initarg  :id
              :reader   no-such-participant-error-id
              :documentation
              "Stores the id for which no participant could be found
               in the container."))
  (:default-initargs
   :container (missing-required-initarg 'no-such-participant-error :container)
   :id        (missing-required-initarg 'no-such-participant-error :id))
  (:report
   (lambda (condition stream)
     (format stream "~@<No participant ~A in container ~A.~@:>"
             (no-such-participant-error-id        condition)
             (no-such-participant-error-container condition))))
  (:documentation
   "This error is signaled when an attempt to retrieve a participant
    by ids id fails."))
