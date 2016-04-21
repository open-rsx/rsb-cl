;;;; types.lisp --- Types used in the transport module.
;;;;
;;;; Copyright (C) 2012, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

;;; Notifications

(deftype notification-index ()
  `(unsigned-byte 32))

(defstruct (wire-notification
             (:constructor make-wire-notification (buffer end))
             (:copier      nil))
  (buffer nil :type nibbles:octet-vector :read-only t)
  (end    nil :type notification-index   :read-only t))
