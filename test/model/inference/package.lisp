;;;; package.lisp --- Package definition for unit tests of the model.inference module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.model.inference.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:fiveam

   #:rsb.model
   #:rsb.model.inference

   #:rsb.test
   #:rsb.model.test)

  (:import-from #:rsb
   #:make-scope)

  (:import-from #:rsb.model

   #:basic-participant-node
   #:basic-process-node
   #:basic-host-node)

  (:import-from #:rsb.model.inference
   #:tri-and #:tri-or)

  (:shadowing-import-from #:rsb.model.test
   #:run-tests)

  ;; Root test suite
  (:export
   #:rsb-model-inference-root)

  (:documentation
   "This package contains unit tests for the introspection module."))

(cl:in-package #:rsb.model.inference.test)

;;; Test suite

(def-suite rsb-model-inference-root
  :in rsb-model-root
  :description
  "Root unit test suite of the model.inference module.")
