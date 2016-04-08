;;;; adjust-timestamps.lisp --- Unit tests for the adjust-timestamps transform.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform.test)

(def-suite rsb.transform.adjust-timestamps-root
  :in rsb.transform-root
  :description
  "Test suite for the `adjust-timestamps' transform.")
(in-suite rsb.transform.adjust-timestamps-root)

(test construct
  "Test constructing `adjust-timestamps' instances."

  (mapc
   (lambda+ ((initargs expected))
     (flet ((do-it ()
              (apply #'make-transform :adjust-timestamps initargs)))
       (case expected
         (transform-creation-error
          (signals transform-creation-error (do-it)))
         (t
          (do-it)))))

   `( ;; Some invalid cases.
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
     ((:adjustments ((:send   :self)))                          t))))

(test smoke
  "Smoke test for the `adjust-timestamps' transform."

  (mapc (lambda+ ((adjustments expected))
          (call-with-transform-checking-thunk
           (lambda (do-it)
             (case expected
               (error (signals error (funcall do-it)))
               (t     (let ((result (funcall do-it)))
                        (dolist (expected expected)
                          (is (typep (timestamp result expected)
                                     'local-time:timestamp)))))))
           (list :adjust-timestamps :adjustments adjustments)
           (list "/" 5)))

        `( ;; Some invalid cases.
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
           (:create)))))

(test print
  "Test printing `adjust-timestamps' instances."

  (let ((transform (make-transform :adjust-timestamps
                                   :adjustments '((:create (+ :self 1))
                                                  (:copy-of-send :send)))))
    (is (search "CREATE ← (+ SELF 1)" (princ-to-string transform)))
    (is (search "COPY-OF-SEND ← SEND" (princ-to-string transform)))))
