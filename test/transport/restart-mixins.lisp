;;;; restart-mixins.lisp --- Unit tests for restart mixins.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

;;; Tests for `restart-notification-receiver-mixin' class

(def-suite restart-notification-receiver-mixin-root
  :in transport-root
  :description
  "Unit tests for the `restart-notification-receiver-mixin' class.")
(in-suite restart-notification-receiver-mixin-root)

(define-restart-test-method receive-notification
    ((connector restart-notification-receiver-mixin)
     (block?    t)))

(define-restart-test-case (receive-notification/restart)
  (let ((connector (make-instance 'restart-notification-receiver-mixin)))
    (receive-notification connector t)))

(define-restart-test-method notification->event
    ((connector    restart-notification-receiver-mixin)
     (notification t)
     (wire-schema  t)))

(define-restart-test-case (notification->event/restart)
  (let ((connector (make-instance 'restart-notification-receiver-mixin)))
    (notification->event connector :does-not-matter :likewise)))

;;; Tests for `restart-notification-sender-mixin' class

(def-suite restart-notification-sender-mixin-root
  :in transport-root
  :description
  "Unit tests for the `restart-notification-sender-mixin' class.")
(in-suite restart-notification-sender-mixin-root)

(define-restart-test-method send-notification
    ((connector    restart-notification-sender-mixin)
     (notification t)))

(define-restart-test-case (send-notification/restart)
  (let ((connector (make-instance 'restart-notification-sender-mixin)))
    (send-notification connector :ignored)))

(define-restart-test-method event->notification
    ((connector    restart-notification-sender-mixin)
     (notification t)))

(define-restart-test-case (event->notification/restart)
  (let ((connector (make-instance 'restart-notification-sender-mixin)))
    (event->notification connector :does-not-matter)))
