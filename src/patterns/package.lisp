;;;; package.lisp --- Package definition for the patterns module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns
  (:use
   #:cl

   #:rsb)

  (:documentation
   "This package contains communication patterns implemented on top of
    the event-driven architecture at the core of RSB.

    Currently the following patterns are supported:

    request-reply (package rsb.patterns.request-reply)

      In this communication pattern, a client submits requests to a
      servers which processes the requests and sends associated
      replies to the client."))
