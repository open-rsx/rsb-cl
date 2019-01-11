;;;; sender-receiver.lisp --- Unit tests for message sender/receiver classes.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

;;; `message-receiver'

(deftestsuite message-receiver-root (transport-spread-root)
  ((a-string           (octetify "foobarbaz"))
   (empty-notification (nth-value 1 (pb:pack (make-instance
                                              'rsb.protocol:notification)))))
  (:documentation
   "Tests for the `message-receiver' class and associated methods."))

(addtest (message-receiver-root
          :documentation
          "Test `notification->event' method.")
  notification->event

  (ensure-cases (notification wire-schema args expected)
      `(;; In these cases, protocol buffer unpacking fails.
        (,a-string           :foo (:error-policy nil)          decoding-error)
        (,a-string           :foo (:error-policy ,#'continue)  nil)

        ;; Protocol buffer unpacking succeeds, but conversion to event
        ;; fails.
        (,empty-notification :foo (:error-policy nil)          decoding-error)
        (,empty-notification :foo (:error-policy ,#'continue)  nil))

    (let+ ((connector    (apply #'make-instance 'message-receiver
                                :connection nil args))
           (notification (rsb.transport:make-wire-notification
                          notification (length notification)))
           ((&flet do-it ()
              (rsb.ep:with-error-policy (connector)
                (notification->event connector notification wire-schema)))))
      (case expected
        (decoding-error (ensure-condition 'decoding-error (do-it)))
        ((nil)          (ensure-null (do-it)))))))

;;; `message-sender'

(deftestsuite message-sender-root (transport-spread-root)
  ()
  (:documentation
   "Tests for the `message-sender' class and associated methods."))

(addtest (message-sender-root
          :documentation
          "Test `event->notification' method.")
  event->notification

  (let* ((connector    (apply #'make-instance 'message-sender
                              :connection nil '()))
         (data         (octetify "bar"))
         (notification (rsb.transport.spread::make-outgoing-notification
                        (make-scope "/foo") '() (a-notification 0 data) data)))
    (collect-fragments (event->notification connector notification))))
