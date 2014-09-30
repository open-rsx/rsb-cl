;;;; conditions.lisp --- Conditions used by the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

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
