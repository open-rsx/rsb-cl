;;;; util.lisp --- Unit tests for utilities used by the cl-rsb system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite curry/weak-root (root)
  ()
  (:documentation
   "Test suite for the `curry/weak' function."))

(addtest (curry/weak-root
          :documentation
          "Smoke test for the `curry/weak' function.")
  smoke

  (let+ ((f (rsb::curry/weak #'1+ 1))
         ((&values result result?) (funcall f)))
    (when result? (ensure-same result 2)))

  (let+ ((f (rsb::curry/weak #'+ 1))
         ((&values result result?) (funcall f 2)))
    (when result? (ensure-same result 3))))

(macrolet
    ((define-construction-cases (class)
       `(ensure-cases (initargs expected)
            `((()                                                                  missing-required-initarg)
              ((:interval 1)                                                       missing-required-initarg)
              ((:interval 1 :function ,,'#'+)                                      t)
              ((:interval 1 :function ,,'#'print :args (list 1 *standard-output*)) t))

          (let+ (((&flet do-it ()
                    (detach (apply #'make-instance ',class initargs)))))
            (case expected
              (missing-required-initarg
               (ensure-condition missing-required-initarg
                 (do-it)))
              (t
               (do-it)))))))

  (deftestsuite timed-executor-root (root)
    ()
    (:documentation
     "Test suite for the `timed-executor' class."))

  (addtest (timed-executor-root
            :documentation
            "Test constructing `timed-executor' instances.")
    construction

    (define-construction-cases timed-executor))

  (deftestsuite timed-executor/weak-root (root)
    ()
    (:documentation
     "Test suite for the `timed-executor/weak' class."))

  (addtest (timed-executor/weak-root
            :documentation
            "Test constructing `timed-executor/weak' instances.")
    construction

    (define-construction-cases timed-executor/weak)))
