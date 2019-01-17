;;;; threaded-receiver-mixin.lisp --- Unit tests for the threaded-receive-mixin class.
;;;;
;;;; Copyright (C) 2011-2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

(defclass mock-receiver (threaded-receiver-mixin) ())

(defmethod receive-messages ((receiver mock-receiver))
  ;; This causes a timeout of the test case, if the interruption does
  ;; not work properly.
  (sleep most-positive-fixnum))

(def-suite* threaded-receiver-mixin-root
  :in transport-root
  :description
  "Unit tests for the `threaded-receiver-mixin' class.")

(test smoke
  "Smoke test for the `threaded-receiver-mixin' class."

  ;; We try attaching and detaching with different timing behaviors.
  (let ((receiver (make-instance 'mock-receiver)))
    (iter (repeat 4)
          (start-receiver receiver)
          (stop-receiver receiver)
          (is (null (connector-thread receiver)))

          (start-receiver receiver)
          (sleep .001)
          (stop-receiver receiver)
          (is (null (connector-thread receiver))))))
