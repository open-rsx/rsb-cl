;;;; package.lisp --- Package definition for unit tests of the patterns.request-reply module.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.request-reply.test
  (:use
   #:cl
   #:let-plus
   #:iterate
   #:more-conditions

   #:fiveam

   #:rsb
   #:rsb.patterns.request-reply

   #:rsb.test
   #:rsb.patterns.test)

  (:shadow
   #:run-tests)

  (:import-from #:rsb.patterns.request-reply
   #:method1
   #:local-method
   #:remote-method)

  (:export
   #:patterns-request-reply-root

   #:run-tests)

  (:documentation
   "This package contains unit tests for the patterns.request-reply
    module."))

(cl:in-package #:rsb.patterns.request-reply.test)

(def-suite patterns-request-reply-root
  :description
  "Root unit test suite for the patterns.request-reply module.")

(defun run-tests ()
  (run! 'patterns-request-reply-root))
