;;;; package.lisp --- Package definition for unit tests of the model.inference module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.model.inference.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:lift

   #:rsb.model
   #:rsb.model.inference

   #:rsb.test)

  (:import-from #:rsb
   #:make-scope)

  (:import-from #:rsb.model

   #:basic-participant-node
   #:basic-process-node
   #:basic-host-node)

  (:import-from #:rsb.model.inference
   #:tri-and #:tri-or)

  ;; Root test suite
  (:export
   #:rsb-model-inference-root)

  (:documentation
   "This package contains unit tests for the introspection module."))

(cl:in-package #:rsb.model.inference.test)

;;; Test suite

(deftestsuite rsb-model-inference-root (rsb-model-root)
  ()
  (:documentation
   "Root unit test suite of the model.inference module."))
