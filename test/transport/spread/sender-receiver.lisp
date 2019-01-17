;;;; sender-receiver.lisp --- Unit tests for message sender/receiver classes.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

;;; `message-receiver'

(def-suite* message-receiver-root
  :in transport-spread-root
  :description
  "Tests for the `message-receiver' class and associated methods.")

;; (empty-notification (nth-value 1 (pb:pack (make-instance
;;                                            'rsb.protocol:notification))))

(test message-receiver/notification->event
  "Test `notification->event' method."

  (mapc
   (lambda+ ((notification wire-schema args expected))
     (let+ ((connector    (apply #'make-instance 'message-receiver
                                 :connection nil args))
            (notification (rsb.transport:make-wire-notification
                           notification (length notification)))
            ((&flet do-it ()
               (rsb.ep:with-error-policy (connector)
                 (notification->event connector notification wire-schema)))))
       (case expected
         (decoding-error (signals decoding-error (do-it)))
         ((nil)          (is (null (do-it)))))))

   `(;; In these cases, protocol buffer unpacking fails.
     (,(octetify "foobarbaz") :foo (:error-policy nil)          decoding-error)
     (,(octetify "foobarbaz") :foo (:error-policy ,#'continue)  nil)

     ;; Protocol buffer unpacking succeeds, but conversion to event
     ;; fails.
     (,empty-notification     :foo (:error-policy nil)          decoding-error)
     (,empty-notification     :foo (:error-policy ,#'continue)  nil))))

;;; `message-sender'

(def-suite* message-sender-root
  :in transport-spread-root
  :description
  "Tests for the `message-sender' class and associated methods.")

(test message-sender/event->notification
  "Test `event->notification' method."

  (let* ((connector    (apply #'make-instance 'message-sender
                              :connection nil '()))
         (data         (octetify "bar"))
         (notification (rsb.transport.spread::make-outgoing-notification
                        (make-scope "/foo") '() (a-notification 0 data) data)))
    (collect-fragments (event->notification connector notification))))
