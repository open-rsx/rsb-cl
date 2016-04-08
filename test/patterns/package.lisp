;;;; package.lisp --- Package definition for unit tests of the patterns module.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:rsb
   #:rsb.patterns

   #:rsb.test)

  (:export
   #:patterns-root)

  (:documentation
   "This package contains unit tests for the patterns module."))

(cl:in-package #:rsb.patterns.test)

(def-suite patterns-root
  :in root
  :description
  "Root unit test suite for the patterns module.")
