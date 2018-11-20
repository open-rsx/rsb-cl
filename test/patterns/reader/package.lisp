;;;; package.lisp --- Package definition for unit tests of the patterns.reader module.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.reader.test
  (:use
   #:cl
   #:lift

   #:rsb
   #:rsb.patterns.reader

   #:rsb.test
   #:rsb.patterns.test)

  (:export
   #:patterns-reader-root)

  (:documentation
   "This package contains unit tests for the patterns.reader
    module."))

(cl:in-package #:rsb.patterns.reader.test)

(deftestsuite patterns-reader-root (patterns-root)
  ()
  (:documentation
   "Root unit test suite for the patterns.reader module."))
