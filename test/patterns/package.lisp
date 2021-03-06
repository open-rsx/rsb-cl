;;;; package.lisp --- Package definition for unit tests of the patterns module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:lift

   #:rsb
   #:rsb.patterns

   #:rsb.test)

  (:export
   #:patterns-root)

  (:documentation
   "This package contains unit tests for the patterns module."))

(cl:in-package #:rsb.patterns.test)

(deftestsuite patterns-root (root)
  ()
  (:documentation
   "Root unit test suite for the patterns module."))
