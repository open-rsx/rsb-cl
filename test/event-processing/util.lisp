;;;; util.lisp --- Unit tests for utility functions of the event-processing module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

(def-suite util-root
  :in event-processing-root
  :description
  "Unit tests for the utility functions of the event-processing module.")

(test merge-implementation-infos-smoke
  "Smoke test for the `merge-implementation-info' function."

  (mapc (lambda+ ((input expected))
          (let ((result (reduce #'merge-implementation-infos input)))
            (is (eq result expected))))

        '((()                              :implemented)
          ((:not-implemented)              :not-implemented)
          ((:implemented)                  :implemented)
          ((:implemented :not-implemented) :not-implemented)
          ((:implemented :implemented)     :implemented))))
