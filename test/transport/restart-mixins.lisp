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

(define-restart-method-test-case
    (receive-notification ((connector restart-notification-receiver-mixin)
                           (block?    t))
     :suite-name restart-notification-receiver-mixin-root)
  (receive-notification simple-connector t))

(define-restart-method-test-case
    (notification->event ((connector    restart-notification-receiver-mixin)
                          (notification t)
                          (wire-schema  t))
     :suite-name restart-notification-receiver-mixin-root)
  (notification->event simple-connector :does-not-matter :likewise))

;;; Tests for `restart-notification-sender-mixin' class

(deftestsuite restart-notification-sender-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-notification-sender-mixin)))
  (:documentation
   "Unit tests for the `restart-notification-sender-mixin' class."))

(define-restart-method-test-case
    (send-notification ((connector    restart-notification-sender-mixin)
                        (notification t))
     :suite-name restart-notification-sender-mixin-root)
  (send-notification simple-connector :ignored))

(define-restart-method-test-case
    (event->notification ((connector    restart-notification-sender-mixin)
                          (notification t))
     :suite-name restart-notification-sender-mixin-root)
  (event->notification simple-connector :does-not-matter))
