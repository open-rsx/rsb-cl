;;;; package.lisp --- Package definition for unit tests of the patterns module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsb.patterns.test
  (:use
   :cl
   :let-plus
   :iterate
   :more-conditions
   :lift

   :rsb
   :rsb.patterns

   :rsb.test)

  (:import-from :rsb.patterns
   :method1
   :local-method)

  (:documentation
   "This package contains unit tests for the patterns module of
cl-rsb."))

(cl:in-package :rsb.patterns.test)

(deftestsuite patterns-root (root)
  ()
  (:documentation
   "Root unit test suite for the patterns module of cl-rsb."))
