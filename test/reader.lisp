;;;; reader.lisp --- Unit tests for the reader class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.test)

(deftestsuite reader-root (root
			   participant-suite)
  ()
  (:documentation
   "Unit tests for the `reader' class and `make-reader' function."))

(define-basic-participant-test-cases :reader
  '("/reader/construction"
    nil
    "/reader/construction")
  '("/reader/construction"
    (:transports ((:inprocess &inherit)))
    "/reader/construction")
  '("/reader/construction"
    (:transports ((t) (:inprocess &inherit)))
    "/reader/construction")
  '("/reader/construction"
    (:converters ((t . :foo)))
    "/reader/construction")
  `("/reader/construction"
    (:transform ,#'1+)
    "/reader/construction")
  '("inprocess:/reader/construction"
    nil
    "/reader/construction")

  ;; No transports => error
  '("/reader/construction"
    (:transports nil)
    :error))

(addtest (reader-root
          :documentation
	  "Test receiving data sent by an informer in a blocking
mode.")
  receive/blocking

  (with-informer (informer "/reader/receive/blocking" t)
    (with-reader (reader "/reader/receive/blocking")
      (ensure-random-cases 32 ((data a-string))
	(send informer data)
	(check-event (receive reader :block? t)
		     "/reader/receive/blocking" data)))))

(addtest (reader-root
          :documentation
	  "Test receiving data sent by an informer in a non-blocking
mode.")
  receive/non-blocking

  (with-informer (informer "/reader/receive/non-blocking" t)
    (with-reader (reader "/reader/receive/non-blocking")
      (ensure-random-cases 32 ((data a-string))
	(send informer data)
	(let ((received
	       (iter (for received next (receive reader :block? nil))
		     (when received
		       (return received)))))
	  (check-event received
		       "/reader/receive/non-blocking" data))))))

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
