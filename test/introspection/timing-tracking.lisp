;;;; timing-tracking.lisp --- Unit tests for the timing tracking functionality.
;;;;
;;;; Copyright (C) 2014, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(def-suite* timing-tracker-root
  :in introspection-root
  :description
  "Unit test suite for the `timing-tracker' class.")

(test timing-tracker/smoke
  "Smoke test for the `timing-tracker' class."

  (let* ((tracker (make-instance 'rsb.introspection::timing-tracker))
         (event   (rsb:make-event
                   "/" 1
                   :timestamps `(:request.send    ,(local-time:parse-timestring
                                                    "2014-10-06T08:03:16.000000+02:00")
                                 :request.receive ,(local-time:parse-timestring
                                                    "2014-10-06T08:03:16.001000+02:00")
                                 :send            ,(local-time:parse-timestring
                                                    "2014-10-06T08:03:16.003000+02:00")
                                 :receive         ,(local-time:parse-timestring
                                                    "2014-10-06T08:03:16.004000+02:00")))))
    (loop :repeat 10 :do (rsb.ep:handle tracker event))
    (is (= 0       (rsb.introspection::timing-tracker-clock-offset tracker)))
    (is (= 0.001d0 (rsb.introspection::timing-tracker-latency tracker)))))
