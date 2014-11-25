;;;; package.lisp --- Package definition for transform module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transform
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rsb)

  ;; Conditions
  (:export
   #:transform-error

   #:transform-error-transform
   #:transform-error-object)

  ;; Transformation protocol
  (:export
   #:transform!)

  (:documentation
   "This package contains event transformation infrastructure and transforms.

    Given an instantiated transform the generic function `transform!'
    can be used to destructively transform events."))
