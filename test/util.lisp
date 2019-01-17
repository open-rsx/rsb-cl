;;;; util.lisp --- Unit tests for utilities used by the rsb system.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(def-suite* curry/weak-root
  :in root
  :description "Test suite for the `curry/weak' function.")

(test smoke
  "Smoke test for the `curry/weak' function."

  (let+ ((f (rsb::curry/weak #'1+ 1))
         ((&values result result?) (funcall f)))
    (when result? (is (eql 2 result))))

  (let+ ((f (rsb::curry/weak #'+ 1))
         ((&values result result?) (funcall f 2)))
    (when result? (is (eql 3 result)))))

(macrolet
    ((define-construction-cases (class)
       `(mapc
         (lambda+ ((initargs expected))
           (let+ (((&flet do-it ()
                     (detach (apply #'make-instance ',class initargs)))))
             (case expected
               (missing-required-initarg
                (signals missing-required-initarg (do-it)))
               (t
                (do-it)))))

         `((()                                                                  missing-required-initarg)
           ((:interval 1)                                                       missing-required-initarg)
           ((:interval 1 :function ,,'#'+)                                      t)
           ((:interval 1 :function ,,'#'print :args (list 1 *standard-output*)) t)))))

  (def-suite* timed-executor-root
    :in root
    :description
    "Test suite for the `timed-executor' class.")

  (test construction
    "Test constructing `timed-executor' instances."

    (define-construction-cases timed-executor))

  (def-suite* timed-executor/weak-root
    :in root
    :description
    "Test suite for the `timed-executor/weak' class.")

  (test construction
    "Test constructing `timed-executor/weak' instances."

    (define-construction-cases timed-executor/weak)))
