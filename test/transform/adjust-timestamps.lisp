;;;; adjust-timestamps.lisp --- Unit tests for the adjust-timestamps transform.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform.test)

(deftestsuite rsb.transform.adjust-timestamps-root (rsb.transform-root)
  ()
  (:documentation
   "Test suite for the `adjust-timestamps' transform."))

(addtest (rsb.transform.adjust-timestamps-root
          :documentation
          "Test constructing `adjust-timestamps' instances.")
  construct

  (ensure-cases (initargs expected)
      `(;; Some invalid cases.
        ((:adjustments 1)                                          transform-creation-error)
        ((:adjustments :foo)                                       transform-creation-error)
        ((:adjustments (:foo))                                     transform-creation-error)
        ((:adjustments ((:foo)))                                   transform-creation-error)
        ((:adjustments ((:create (:copy))))                        transform-creation-error)
        ((:adjustments ((:create (+))))                            transform-creation-error)
        ((:adjustments ((:create (+ 1))))                          transform-creation-error)

        ;; These are OK.
        (()                                                        t)
        ((:adjustments ())                                         t)
        ((:adjustments ((:create :now)))                           t)
        ((:adjustments ((:create ,(local-time:now))))              t)
        ((:adjustments ((:create ,(local-time:now)) (:send :now))) t)
        ((:adjustments ((:send   (:copy :create))))                t)
        ((:adjustments ((:send   (+ :create 5))))                  t)
        ((:adjustments ((:send   (+ (+ :create -1) 5))))           t)
        ((:adjustments ((:send   :self)))                          t))

    (flet ((do-it ()
             (apply #'make-transform :adjust-timestamps initargs)))
      (case expected
        (transform-creation-error
         (ensure-condition 'transform-creation-error (do-it)))
        (t
         (do-it))))))

(addtest (rsb.transform.adjust-timestamps-root
          :documentation
          "Smoke test for the `adjust-timestamps' transform.")
  smoke

  (ensure-cases (adjustments expected)
      `(;; Some invalid cases.
        (((:copied :no-such-timestamp)) error)

        ;; These are OK.
        (()
         (:create))

        (((:fresh ,(local-time:now)))
         (:create :fresh))

        (((:copied :now))
         (:create :copied))

        (((:copied (:copy :create)))
         (:create :copied))

        (((:copied-1 :now) (:copied-2 :copied-1))
         (:create :copied-1 :copied-2))

        (((:adjusted (+ :create 5)))
         (:create :adjusted))

        (((:create (+ :self -1)))
         (:create)))

    (let+ ((event     (make-event "/" 5))
           (transform (make-transform :adjust-timestamps
                                      :adjustments adjustments))
           ((&flet do-it ()
              (transform! transform event))))
      (case expected
        (error (ensure-condition 'error (do-it)))
        (t     (let ((result (do-it)))
                 (dolist (expected expected)
                   (ensure (typep (timestamp result expected)
                                  'local-time:timestamp)))))))))

(addtest (rsb.transform.adjust-timestamps-root
          :documentation
          "Test printing `adjust-timestamps' instances.")
  print

  (let ((transform (make-transform :adjust-timestamps
                                   :adjustments '((:create (+ :self 1))
                                                  (:copy-of-send :send)))))
    (ensure (search "CREATE ← (+ SELF 1)" (princ-to-string transform)))
    (ensure (search "COPY-OF-SEND ← SEND" (princ-to-string transform)))))
