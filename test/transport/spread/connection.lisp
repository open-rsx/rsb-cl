;;;; connection.lisp --- Unit tests for the connection class.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

;;; Utilities

(defun check-connection (connection expected-groups)
  (flet ((set-equal/string (left right)
           (set-equal left right :test #'string=)))
    (is (set-equal/string expected-groups (connection-groups connection)))))

;;; Tests

(def-suite* spread-connection-root
  :in transport-spread-root
  :description
  "Unit tests for the `connection' class.")

(test connection/construct
  "Test construction of `connection' instances."

  (let+ ((name (format nil "~D" *spread-port*))
         ((&flet connect ()
            (network.spread:connect name))))
    (mapc (lambda+ ((initargs expected))
            (let+ (((&flet+ do-it ()
                      (apply #'make-instance 'connection initargs))))
              (case expected
                (missing-required-initarg
                 (signals missing-required-initarg (do-it)))
                (incompatible-initargs
                 (signals incompatible-initargs (do-it)))
                (t
                 (detach (do-it))))))

          `( ;; Some invalid cases.
            (()                            missing-required-initarg) ; Neither :connection nor :name
            ((:name       "3333@localhost"
              :connection ,(connect))      incompatible-initargs) ; Both :connection and :name

            ;; These are OK.
            ((:connection ,(connect))      t)
            ((:name ,name)                 t)))))

(test connection/print
  "Test printing `connection' instances."

  (with-connection (connection :port *spread-port*)
    (is-false (emptyp (princ-to-string connection)))))

(test ref/unref-group.smoke
  "Smoke test for `ref-group' and `unref-group' methods."

  (with-connection (connection :port *spread-port*)
    (let ((group *simple-group-name*))
      (is (equal (values 1 1 t) (ref-group connection group)) )
      (check-connection connection (list group))

      (is (equal (values 2 1 nil) (ref-group connection group)))
      (check-connection connection (list group))

      (is (equal (values 1 1 nil) (unref-group connection group)))
      (check-connection connection (list group))

      (is (equal (values 0 0 t) (unref-group connection group)))
      (check-connection connection '())

      ;; Invalid unreference should signal an error.
      (signals error (unref-group connection group)))))

(test ref/unref-group.waitatble
  "Test for `ref-group' and `unref-group' methods in waitable mode."

  (with-connection (connection :port *spread-port*)
    (let+ ((group *simple-group-name*)
           ((&flet ref ()
              (ref-group connection group :waitable? t)))
           ((&flet unref ()
              (unref-group connection group :waitable? t))))

      (let+ (((&values member-count group-count promise) (ref)))
        (is (eql 1 member-count))
        (is (eql 1 group-count))
        (loop :until (lparallel:fulfilledp promise)
              :do (receive-message connection nil)))
      (check-connection connection (list group))

      (is (equal (values 2 1 nil) (ref)))
      (check-connection connection (list group))

      (is (equal (values 1 1 nil) (unref)))
      (check-connection connection (list group))

      ;; Make sure the join/leave notification skips unrelated
      ;; messages.
      (let* ((data         (octetify "foo"))
             (notification (make-wire-notification data (length data))))
        (send-message connection (list group) notification))

      (let+ (((&values member-count group-count promise) (unref)))
        (is (eql 0 member-count))
        (is (eql 0 group-count))
        (loop :until (lparallel:fulfilledp promise)
              :do (receive-message connection nil)))
      (check-connection connection '()))))

(test connection/roundtrip
  "Roundtrip test for `connenction' class."

  (let ((group "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (with-connection (sender :port *spread-port*)
      (with-connection (receiver :port *spread-port*)
        ;; Join group and wait until it's done.
        (lparallel:force (ref-group receiver group :waitable? t))
        ;; Then send, receive and verify a message.
        (mapc
         (lambda (payload-size)
           (let* ((payload      (octetify (make-string payload-size
                                                       :initial-element #\b)))
                  (notification (make-wire-notification payload (length payload))))
             (send-message sender (list group) notification)
             (let* ((incoming (receive-message receiver t))
                    (incoming (subseq (wire-notification-buffer incoming)
                                      0 (wire-notification-end incoming))))
               (is (equalp payload incoming)))))
         '(3 1000 100000))))))
