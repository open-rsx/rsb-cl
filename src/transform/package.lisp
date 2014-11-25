;;;; package.lisp --- Package definition for transform module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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
   #:transform-creation-error
   #:transform-creation-error-spec

   #:transform-error

   #:transform-error-transform
   #:transform-error-object)

  ;; Variables
  (:export
   #:+dropped-payload+)

  ;; Transformation protocol
  (:export
   #:transform!)

  ;; Transform creation protocol
  (:export
   #:make-transform)

  (:documentation
   "This package contains event transformation infrastructure and transforms.

    The generic function `make-transform' can be used to instantiate
    transforms.

    Given an instantiated transform the generic function `transform!'
    can be used to destructively transform events."))
