;;;; package.lisp --- Package definition for the transport.inprocess module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsb.transport.inprocess
  (:nicknames
   :rsb.tp.inprocess
   :rsb.tp.inproc)

  (:shadow
   :connector)

  (:use
   :cl
   :alexandria
   :iterate
   :let-plus

   :rsb
   :rsb.event-processing
   :rsb.filter
   :rsb.transport)

  ;; Exported for unit tests
  (:export
   :in-pull-connector
   :in-push-connector
   :out-connector)

  (:documentation
   "This package contains a transport that delivers RSB events within
a process."))
