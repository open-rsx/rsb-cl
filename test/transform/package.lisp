;;;; package.lisp --- Package definition for unit tests of the transform module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transform.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:rsb.transform

   #:rsb.test)

  (:documentation
   "This package contains unit tests for the transform module"))

(cl:in-package #:rsb.transform.test)

(deftestsuite rsb.transform-root (root)
  ()
  (:documentation
   "Root unit test suite for the transform module."))
