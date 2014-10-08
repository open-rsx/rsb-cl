;;;; reader.lisp --- Unit tests for the reader class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite reader-root (root
                           participant-suite)
  ()
  (:documentation
   "Unit tests for the `reader' class and `make-reader' function."))

(define-basic-participant-test-cases reader
  '("/rsbtest/reader/construction"
    () () ()
    "/rsbtest/reader/construction")

  '("/rsbtest/reader/construction"
    () () (:transports ((:inprocess &inherit)))
    "/rsbtest/reader/construction")

  #+TODO-disabled '("/rsbtest/reader/construction"
                    () () (:transports ((t) (:inprocess &inherit)))
                    "/rsbtest/reader/construction")

  '("/rsbtest/reader/construction"
    () () (:converters ((t . :foo)))
    "/rsbtest/reader/construction")

  `("/rsbtest/reader/construction"
    () () (:transform ,#'1+)
    "/rsbtest/reader/construction")

  '("inprocess:/rsbtest/reader/construction"
    () () ()
    "/rsbtest/reader/construction")

  `("inprocess:/rsbtest/reader/construction"
    () () (:error-policy ,#'continue)
    "/rsbtest/reader/construction")

  `("/rsbtest/reader/construction?foo=bar"
    () () ()
    "/rsbtest/reader/construction")

  ;; No transports => error
  '("/rsbtest/reader/construction"
    () () (:transports ((t :enabled nil)))
    error))

(addtest (reader-root
          :documentation
          "Test receiving data sent by an informer in a blocking
mode.")
  receive/blocking

  (with-informer (informer "/rsbtest/reader/receive/blocking" t)
    (with-reader (reader "/rsbtest/reader/receive/blocking")
      (ensure-random-cases 32 ((data a-string))
        (send informer data)
        (check-event (receive reader :block? t)
                     "/rsbtest/reader/receive/blocking" data)))))

(addtest (reader-root
          :documentation
          "Test receiving data sent by an informer in a non-blocking
mode.")
  receive/non-blocking

  (with-informer (informer "/rsbtest/reader/receive/non-blocking" t)
    (with-reader (reader "/rsbtest/reader/receive/non-blocking")
      (ensure-random-cases 32 ((data a-string))
        (send informer data)
        (let ((received
               (iter (for received next (receive reader :block? nil))
                     (when received
                       (return received)))))
          (check-event received
                       "/rsbtest/reader/receive/non-blocking" data))))))

(define-error-hook-test-case (reader)
  ;; Force an error during dispatch by injecting a signaling
  ;; pseudo-filter.
  (push (lambda (event)
          (let ((error (make-condition 'simple-error
                                       :format-control   "I hate ~A"
                                       :format-arguments (list event))))
            (push error expected-errors)
            (error error)))
        (receiver-filters reader))

  ;; Try to send and receive an event to trigger the error.
  (send informer "foo")
  (receive reader :block? nil))
