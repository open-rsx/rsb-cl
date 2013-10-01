;;;; restart-mixins.lisp --- Unit tests for restart mixins.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

;;; Tests for `restart-message-receiver-mixin' class

(deftestsuite restart-message-receiver-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-message-receiver-mixin)))
  (:documentation
   "Unit tests for the `restart-message-receiver-mixin' class."))

(define-restart-method-test-case
    (restart-message-receiver-mixin
     receive-message (connector (block? t)))
  (receive-message simple-connector t))

(define-restart-method-test-case
    (restart-message-receiver-mixin
     message->event (connector (message t) (wire-schema t)))
  (message->event simple-connector :does-not-matter :likewise))

;;; Tests for `restart-notification-sender-mixin' class

(deftestsuite restart-notification-sender-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-notification-sender-mixin)))
  (:documentation
   "Unit tests for the `restart-notification-sender-mixin' class."))

(define-restart-method-test-case
    (restart-notification-sender-mixin
     send-notification (connector (notification t)))
    (send-notification simple-connector :ignored))

(define-restart-method-test-case
    (restart-notification-sender-mixin
     event->notification (connector (notification t)))
    (event->notification simple-connector :does-not-matter))
