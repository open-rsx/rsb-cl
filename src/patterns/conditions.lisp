;;;; conditions.lisp --- Conditions provided by the patterns module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns)

;;; Conditions related to composite participants

(define-condition child-condition (condition)
  ((container :initarg  :container
              :reader   child-condition-container
              :documentation
              "Stores the container participant in which the requested
               child participant could not be found.")
   (key       :initarg  :key
              :type     cons
              :reader   child-condition-key
              :documentation
              "Stores the key naming the requested child.

               The key is usually a list of a name and a kind."))
  (:default-initargs
   :container (missing-required-initarg 'child-condition :container)
   :key       (missing-required-initarg 'child-condition :key))
  (:documentation
   "Superclass for conditions involving a child participant and a
    containing parent participant."))

(define-condition no-such-child-error (rsb-error
                                       child-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The child participant ~{~S~^ ~} could not be ~
                     found in ~A.~@:>"
             (child-condition-key       condition)
             (child-condition-container condition))))
  (:documentation
   "This error is signaled when a requested child participant cannot
    be found in a container participant."))

(define-condition child-exists-error (rsb-error
                                      child-condition)
  ((child :initarg  :child
          :reader   child-exists-error-child
          :documentation
          "Stores the duplicate child participant."))
  (:default-initargs
   :child (missing-required-initarg 'child-exists-error :child))
  (:report
   (lambda (condition stream)
     (format stream "~@<There already is the child participant ~A ~
                     designated by ~{~S~^ ~} in ~A.~@:>"
             (child-exists-error-child  condition)
             (child-condition-key       condition)
             (child-condition-container condition))))
  (:documentation
   "This error is signaled when an attempt is made to add a child
    participant to a container participant and a child participant of
    the same name already exists in the container participant."))

;;; Protocol-related conditions

(define-condition protocol-condition (rsb-condition
                                      chainable-condition)
  ((protocol :initarg  :protocol
             :reader   protocol-condition-protocol
             :documentation
             "Stores a designator of the protocol in the context of
              which the message has been exchanged.")
   (role     :initarg  :role
             :reader   protocol-condition-role
             :initform nil
             :documentation
             "Stores a designator of the role the message would play
              in the protocol.")
   (message  :initarg  :message
             :reader   protocol-condition-message
             :documentation
             "Stores the offending message."))
  (:default-initargs
   :protocol (missing-required-initarg 'protocol-condition :protocol)
   :message  (missing-required-initarg 'protocol-condition :message))
  (:report
   (lambda (condition stream)
     (format stream "~@<The message ~A does not conform to ~
                     ~@[the ~A role within ~]the ~A protocol.~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (protocol-condition-message  condition)
             (protocol-condition-role     condition)
             (protocol-condition-protocol condition)
             condition)))
  (:documentation
   "Subclasses of this condition class indicate violations of
    communication protocols."))

(define-condition simple-protocol-condition (protocol-condition
                                             simple-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The message ~A does not conform to ~
                     ~@[the ~A role within ~]the ~A protocol~
                     ~/more-conditions:maybe-print-explanation/~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (protocol-condition-message  condition)
             (protocol-condition-role     condition)
             (protocol-condition-protocol condition)
             condition condition)))
  (:documentation
   "Subclasses of this condition class indicate violations of
    communication protocols."))

(define-condition protocol-problem (protocol-condition
                                    rsb-problem-condition)
  ()
  (:documentation
   "Superclass for protocol problem conditions such as warnings and
    errors."))

(define-condition protocol-warning (warning
                                    protocol-condition)
  ()
  (:documentation
   "This warning is signaled when a protocol violations is encountered
    which is not serious enough to warrant an error."))

(define-condition protocol-error (error
                                  protocol-condition)
  ()
  (:documentation
   "This error is signaled if a serious protocol violation is
    encountered."))
