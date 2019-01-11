;;;; package.lisp --- Package definition for unit tests of the transport.inprocess module.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.inprocess.test
  (:use
   #:cl
   #:lift

   #:rsb.transport.inprocess

   #:rsb.transport.test)

  (:import-from #:rsb.transport.inprocess
   #:in-connector
   #:out-connector)

  (:documentation
   "This package contains unit tests for the transport.inprocess
    module."))

(cl:in-package #:rsb.transport.inprocess.test)

(deftestsuite transport-inprocess-root (transport-root)
  ()
  (:documentation
   "Root unit test suite for the transport.inprocess module."))
