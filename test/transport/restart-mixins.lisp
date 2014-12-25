;;;; restart-mixins.lisp --- Unit tests for restart mixins.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

;;; Tests for `restart-notification-receiver-mixin' class

(deftestsuite restart-notification-receiver-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-notification-receiver-mixin)))
  (:documentation
   "Unit tests for the `restart-notification-receiver-mixin' class."))

(define-restart-test-method receive-notification
    ((connector restart-notification-receiver-mixin)
     (block?    t)))

(define-restart-test-case
    (receive-notification/restart
     :suite-name restart-notification-receiver-mixin-root)
  (receive-notification simple-connector t))

(define-restart-test-method notification->event
    ((connector    restart-notification-receiver-mixin)
     (notification t)
     (wire-schema  t)))

(define-restart-test-case
    (notification->event/restart
     :suite-name restart-notification-receiver-mixin-root)
  (notification->event simple-connector :does-not-matter :likewise))

;;; Tests for `restart-notification-sender-mixin' class

(deftestsuite restart-notification-sender-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-notification-sender-mixin)))
  (:documentation
   "Unit tests for the `restart-notification-sender-mixin' class."))

(define-restart-test-method send-notification
    ((connector    restart-notification-sender-mixin)
     (notification t)))

(define-restart-test-case
    (send-notification/restart
     :suite-name restart-notification-sender-mixin-root)
  (send-notification simple-connector :ignored))

(define-restart-test-method event->notification
    ((connector    restart-notification-sender-mixin)
     (notification t)))

(define-restart-test-case
    (event->notification/restart
     :suite-name restart-notification-sender-mixin-root)
  (event->notification simple-connector :does-not-matter))
