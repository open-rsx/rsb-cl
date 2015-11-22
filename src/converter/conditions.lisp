;;;; conditions.lisp --- Conditions used in the converter module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

(define-condition conversion-error (rsb-error
                                    chainable-condition)
  ((wire-schema :initarg  :wire-schema
                :type     symbol
                :reader   conversion-error-wire-schema
                :documentation
                "This wire-schema to or from which the failed
                 conversion would have converted."))
  (:documentation
   "This condition class can be used as a superclass for
    conversion-related condition classes."))

(define-condition wire->domain-conversion-error (conversion-error)
  ((encoded     :initarg  :encoded
                :type     t
                :reader   conversion-error-encoded
                :documentation
                "The wire-data that could not be converted into a
                 domain object.")
   (domain-type :initarg  :domain-type
                :type     t
                :reader   conversion-error-domain-type
                :documentation
                "The type of the domain object object that would have
                 been produced by a successful conversion."))
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o conversion-error- wire-schema encoded domain-type)
             condition)
            ((&values data shortened?) (maybe-shorten-sequence encoded)))
       (format stream "~@<The wire-data ~S~:[~; ...~] (in ~S ~
                       wire-schema) could not be converted to domain ~
                       type ~
                       ~S~/more-conditions:maybe-print-cause/~@:>"
                       data shortened? wire-schema domain-type condition))))
  (:documentation
   "This error is signaled when wire data cannot be converted to a
    domain object."))

(define-condition domain->wire-conversion-error (conversion-error)
  ((domain-object :initarg  :domain-object
                  :type     t
                  :reader   conversion-error-domain-object
                  :documentation
                  "The domain object that could not be converter into
                   a wire representation.")
   (wire-type     :initarg  :wire-type
                  :type     t
                  :reader   conversion-error-wire-type
                  :documentation
                  "The type of the wire-data that would have been
                   produced by a successful conversion."))
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o conversion-error- wire-schema domain-object wire-type)
             condition))
       (format stream "~@<The domain object ~S could not be converted ~
                       to a wire-type ~S representation using the ~
                       wire-schema ~
                       ~S.~/more-conditions:maybe-print-cause/~@:> "
                       domain-object wire-type wire-schema condition))))
  (:documentation
   "This error is signaled when a domain object cannot be converted to
    a wire-type representation."))
