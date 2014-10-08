;;;; listener.lisp --- Unit tests for listener.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite listener-root (root
                             participant-suite)
  ()
  (:function
   (send-some (informer)
     (iter (repeat 100)
           (send informer "foo")
           (send informer "bar"))))
  (:documentation
   "Unit tests for the `listener' class and `make-listener'
function."))

(define-basic-participant-test-cases listener
  '("/rsbtest/listener/construction"
    () () ()
    "/rsbtest/listener/construction")

  '("/rsbtest/listener/construction"
    () () (:transports ((:inprocess &inherit)))
    "/rsbtest/listener/construction")

  '("/rsbtest/listener/construction"
    () () (:transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/listener/construction")

  '("/rsbtest/listener/construction"
    () () (:converters ((t . :foo)))
    "/rsbtest/listener/construction")

  `("/rsbtest/listener/construction"
    () () (:transform ,#'1+)
    "/rsbtest/listener/construction")

  `("/rsbtest/listener/construction"
    () () (:parent ,*simple-parent*)
    "/rsbtest/listener/construction")

  '("/rsbtest/listener/construction"
    () () (:introspection? nil)
    "/rsbtest/listener/construction")

  '("/rsbtest/listener/construction"
    () () (:introspection? t)
    "/rsbtest/listener/construction")

  '("inprocess:/rsbtest/listener/construction"
    () () ()
    "/rsbtest/listener/construction")

  `("inprocess:/rsbtest/listener/construction"
    () () (:error-policy ,#'continue)
    "/rsbtest/listener/construction")

  `("/rsbtest/listener/construction?foo=bar"
    () () ()
    "/rsbtest/listener/construction")

  ;; No transports => error
  '("/rsbtest/listener/construction"
    () () (:transports ((t :enabled nil)))
    error))

(addtest (listener-root
          :documentation
          "Test adding and removing handlers to/from a `listener'
instance.")
  handlers

  (with-listener (listener "/foo")
    ;; Initially, there should not be any handlers.
    (ensure-null (rsb.ep:handlers listener))

    ;; Test adding and removing a handler.
    (let ((handler (lambda (event) (declare (ignore event)))))
      (push handler (rsb.ep:handlers listener))
      (ensure-same (rsb.ep:handlers listener) (list handler)
                   :test #'equal)

      (removef (rsb.ep:handlers listener) handler)
      (ensure-null (rsb.ep:handlers listener)))))

(addtest (listener-root
          :documentation
          "Test receiving data sent by an informer.")
  receive

  (with-informer (informer "/rsbtest/listener/receive" t)
    (with-listener (listener "/rsbtest/listener/receive")
      ;; Test receive
      (send-some informer)

      ;; Test receive with filters
      (push (filter :origin :origin (uuid:make-v1-uuid))
            (receiver-filters listener))
      (send-some informer)

      ;; Test receive with handlers
      (push (lambda (event) (declare (ignore event)))
            (rsb.ep:handlers listener))
      (send-some informer))))

(define-error-hook-test-case (listener)
  ;; Force an error during dispatch by injecting a signaling
  ;; pseudo-filter.
  (push (lambda (event)
          (let ((error (make-condition 'simple-error
                                       :format-control   "I hate ~A"
                                       :format-arguments (list event))))
            (push error expected-errors)
            (error error)))
        (receiver-filters listener))

  ;; Try to send and receive an event to trigger the error.
  (send informer "foo"))
