;;;; connection.lisp --- Unit tests for the connection class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

;;; Utilities

(defun check-connection (connection expected-groups)
  (ensure-same (connection-groups connection) expected-groups
               :test (rcurry #'set-equal :test #'string=)))

;;; Tests

(deftestsuite spread-connection-root (transport-spread-root)
  ()
  (:documentation
   "Unit tests for the `connection' class."))

(addtest (spread-connection-root
          :documentation
          "Test construction of `connection' instances.")
  construct

  (let+ ((name (format nil "~D" spread-port))
         ((&flet connect ()
            (network.spread:connect name))))
    (ensure-cases (initargs expected)
        `(;; Some invalid cases.
          (()                            missing-required-initarg) ; Neither :connection nor :name
          ((:name       "3333@localhost"
            :connection ,(connect))      incompatible-initargs)    ; Both :connection and :name

          ;; These are OK.
          ((:connection ,(connect))      t)
          ((:name ,name)                 t))
      (let+ (((&flet+ do-it ()
                (apply #'make-instance 'connection initargs))))
       (case expected
         (missing-required-initarg
          (ensure-condition 'missing-required-initarg (do-it)))
         (incompatible-initargs
          (ensure-condition 'incompatible-initargs (do-it)))
         (t
          (detach (do-it))))))))

(addtest (spread-connection-root
          :documentation
          "Test printing `connection' instances.")
  print

  (with-connection (connection :port spread-port)
    (ensure (not (emptyp (princ-to-string connection))))))

(addtest (spread-connection-root
          :documentation
          "Smoke test for `ref-group' and `unref-group' methods.")
  ref/unref-group.smoke

  (with-connection (connection :port spread-port)
    (let ((group *simple-group-name*))
      (ensure-same (ref-group connection group) (values 1 1 t))
      (check-connection connection (list group))

      (ensure-same (ref-group connection group) (values 2 1 nil))
      (check-connection connection (list group))

      (ensure-same (unref-group connection group) (values 1 1 nil))
      (check-connection connection (list group))

      (ensure-same (unref-group connection group) (values 0 0 t))
      (check-connection connection '())

      ;; Invalid unreference should signal an error.
      (ensure-condition error (unref-group connection group)))))

(addtest (spread-connection-root
          :documentation
          "Test for `ref-group' and `unref-group' methods in waitable
           mode.")
  ref/unref-group.waitatble

  (with-connection (connection :port spread-port)
    (let+ ((group *simple-group-name*)
           ((&flet ref ()
              (ref-group connection group :waitable? t)))
           ((&flet unref ()
              (unref-group connection group :waitable? t))))

      (let+ (((&values member-count group-count promise) (ref)))
        (ensure-same (values member-count group-count) (values 1 1))
        (loop :until (lparallel:fulfilledp promise)
              :do (receive-message connection nil)))
      (check-connection connection (list group))

      (ensure-same (ref) (values 2 1 nil))
      (check-connection connection (list group))

      (ensure-same (unref) (values 1 1 nil))
      (check-connection connection (list group))

      ;; Make sure the join/leave notification skips unrelated
      ;; messages.
      (let* ((data         (octetify "foo"))
             (notification (make-wire-notification data (length data))))
        (send-message connection (list group) notification))

      (let+ (((&values member-count group-count promise) (unref)))
        (ensure-same (values member-count group-count) (values 0 0 t))
        (loop :until (lparallel:fulfilledp promise)
              :do (receive-message connection nil)))
      (check-connection connection '()))))
