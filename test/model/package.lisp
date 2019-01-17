;;;; package.lisp --- Package definition for unit tests of the model module.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.model.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:fiveam

   #:rsb
   #:rsb.model

   #:rsb.test)

  (:shadow
   #:run-tests)

  (:import-from #:rsb.model

   #:basic-participant-node
   #:basic-process-node
   #:basic-host-node)

  ;; Root test suite
  (:export
   #:rsb-model-root

   #:run-tests)

  (:documentation
   "This package contains unit tests for the model module."))

(cl:in-package #:rsb.model.test)

;;; Test suite

(def-suite rsb-model-root
  :description
  "Root unit test suite of the model module.")

(defun run-tests ()
  (run! 'rsb-model-root))
