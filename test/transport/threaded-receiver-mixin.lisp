;;;; threaded-receiver-mixin.lisp --- Unit tests for the threaded-receive-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

(defclass mock-receiver (threaded-receiver-mixin) ())

(defmethod receive-messages ((receiver mock-receiver))
  ;; This causes a timeout of the test case, if the interruption does
  ;; not work properly.
  (sleep most-positive-fixnum))

(deftestsuite threaded-receiver-mixin-root (transport-root)
  ()
  (:documentation
   "Unit tests for the `threaded-receiver-mixin' class."))

(addtest (threaded-receiver-mixin-root
          :documentation
          "Smoke test for the `threaded-receiver-mixin' class.")
  smoke

  ;; We try attaching and detaching with different timing behaviors.
  (let ((receiver (make-instance 'mock-receiver)))
    (iter (repeat 4)
          (start-receiver receiver)
          (stop-receiver receiver)
          (ensure-null (connector-thread receiver))

          (start-receiver receiver)
          (sleep .001)
          (stop-receiver receiver)
          (ensure-null (connector-thread receiver)))))
