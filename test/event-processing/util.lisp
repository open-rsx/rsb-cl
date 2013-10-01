;;;; util.lisp --- Unit tests for utility functions of the event-processing module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

(deftestsuite util-root (event-processing-root)
  ()
  (:documentation
   "Unit tests for the utility functions of the event-processing module."))

(addtest (util-root
          :documentation
          "Smoke test for the `merge-implementation-info' function.")
  merge-implementation-infos-smoke

  (ensure-cases (input expected)
      '((()                              :implemented)
        ((:not-implemented)              :not-implemented)
        ((:implemented)                  :implemented)
        ((:implemented :not-implemented) :not-implemented)
        ((:implemented :implemented)     :implemented))

    (let ((result (reduce #'merge-implementation-infos input)))
      (ensure-same result expected
                   :test #'eq))))
