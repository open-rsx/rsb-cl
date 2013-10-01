;;;; variables.lisp --- Variables used in the transport module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

(declaim (special *transport-metrics*))

(defvar *transport-metrics*
  '(:rsb.transport.connector
    :rsb.transport.wire-schema
    :rsb.transport.payload-size
    :rsb.transport.notification-size)
  "List of names of transport metrics that can be requested.")
