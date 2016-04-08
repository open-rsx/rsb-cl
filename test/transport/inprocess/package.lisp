;;;; package.lisp --- Package definition for unit tests of the transport.inprocess module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.inprocess.test
  (:use
   #:cl

   #:fiveam

   #:rsb.transport.inprocess

   #:rsb.transport.test)

  (:import-from #:rsb.transport.inprocess
   #:in-pull-connector
   #:in-push-connector
   #:out-connector)

  (:documentation
   "This package contains unit tests for the transport.inprocess
    module."))

(cl:in-package #:rsb.transport.inprocess.test)

(def-suite transport-inprocess-root
  :in transport-root
  :description
  "Root unit test suite for the transport.inprocess module.")
