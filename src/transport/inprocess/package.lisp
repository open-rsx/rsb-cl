;;;; package.lisp --- Package definition for the transport.inprocess module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.inprocess
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus

   #:rsb
   #:rsb.event-processing
   #:rsb.filter
   #:rsb.transport)

  (:shadow
   #:connector)

  (:documentation
   "This package contains a transport that delivers RSB events within
    a process."))
