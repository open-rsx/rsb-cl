;;;; package.lisp --- Package definition for converter module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.converter
  (:nicknames :rsb.conv)
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:nibbles

   #:rsb)

  ;; Conditions
  (:export
   #:conversion-error
   #:conversion-error-wire-schema

   #:wire->domain-conversion-error
   #:conversion-error-encoded
   #:conversion-error-domain-type

   #:domain->wire-conversion-error
   #:conversion-error-domain-object
   #:conversion-error-wire-type)

  ;; Converter protocol
  (:export
   #:domain->wire?
   #:wire->domain?

   #:domain->wire
   #:wire->domain)

  ;; void converter
  (:export
   #:+no-value+ ; marker value
   #:no-value   ; type
   )

  ;; `force-wire-schema' converter class
  (:export
   #:force-wire-schema
   #:converter-wire-schema)

  (:documentation
   "This package contains mechanisms for converting between domain
object (which are Lisp object) and data representation in different
kinds of wire formats."))
