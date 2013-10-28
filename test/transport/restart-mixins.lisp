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
    (receive-message ((connector restart-message-receiver-mixin)
                      (block?    t))
     :suite-name restart-message-receiver-mixin-root)
  (receive-message simple-connector t))

(define-restart-method-test-case
    (message->event ((connector   restart-message-receiver-mixin)
                     (message     t)
                     (wire-schema t))
     :suite-name restart-message-receiver-mixin-root)
  (message->event simple-connector :does-not-matter :likewise))

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
