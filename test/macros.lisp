;;;; macros.lisp --- Unit tests for macros.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite macros-root (root
                           participant-suite)
  ()
  (:documentation
   "Unit tests for macros provided by the cl-rsb system."))

;;; Listener-related macros

(addtest (macros-root
          :documentation
          "Smoke test for the `with-listener' macro.")
  with-listener/smoke

  (with-listener (listener "inprocess:/rsbtest/macros-root/with-listener/smoke")
    (ensure (typep listener 'listener))
    (check-participant listener :listener "/rsbtest/macros-root/with-listener/smoke")))

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-listener' macro.")
  with-listener/error-policy

  (let ((calls    '())
        (received '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-listener (listener "inprocess:/rsbtest/macros-root/with-listener/error-policy"
                                     :transform #'mock-transform/error
                                     ,@(when policy `(:error-policy ,policy)))
              (with-handler listener ((event) (push event received))
                (with-informer (informer "inprocess:/rsbtest/macros-root/with-listener/error-policy" t)
                  (send informer 1))))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition rsb.event-processing:transform-error
        (test-case))

      ;; With `continue' error policy, receiving and dispatching of
      ;; the event should proceed without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure (typep result 'event))
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.event-processing:transform-error))
        (ensure-same (length received) 1)))))

(addtest (macros-root
          :documentation
          "Smoke test for the `with-handler' macro.")
  with-handler/smoke

  (let ((received '()))
    (with-listener (listener "inprocess:/rsbtest/macros-root/with-handler/smoke")
      (with-handler listener ((event) (push event received))
        (ensure (typep listener 'listener))
        (check-participant listener :listener "/rsbtest/macros-root/with-handler/smoke")
        (with-informer (i "inprocess:/rsbtest/macros-root/with-handler/smoke" t) (send i 1))))
    (ensure-same (length received) 1)))

;;; Reader-related macros

(addtest (macros-root
          :documentation
          "Smoke test for the `with-reader' macro.")
  with-reader/smoke

  (with-reader (reader "inprocess:/rsbtest/macros-root/with-reader/smoke")
    (ensure (typep reader 'reader))
    (check-participant reader :reader "/rsbtest/macros-root/with-reader/smoke")))

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-reader' macro.")
  with-reader/error-policy

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-reader (reader "inprocess:/rsbtest/macros-root/with-reader/error-policy"
                                 :transform #'mock-transform/error
                                 ,@(when policy `(:error-policy ,policy)))
              (with-informer (informer "inprocess:/rsbtest/macros-root/with-reader/error-policy" t)
                (send informer 1))
              (receive reader))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition rsb.event-processing:transform-error
        (test-case))

      ;; With `continue' error policy, sending the event should proceed
      ;; without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure (typep result 'event))
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.event-processing:transform-error))))))

;;; Informer-related macros

(addtest (macros-root
          :documentation
          "Smoke test for the `with-informer' macro.")
  with-informer/smoke

  (with-informer (informer "inprocess:/rsbtest/macros-root/with-informer/smoke" t)
    (ensure (typep informer 'informer))
    (check-participant informer :informer "/rsbtest/macros-root/with-informer/smoke")))

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-informer' macro.")
  with-informer/error-policy

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-informer (informer "inprocess:/rsbtest/macros-root/with-informer/error-policy" t
                                     :transform #'mock-transform/error
                                     ,@(when policy `(:error-policy ,policy)))
              (send informer 1))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition rsb.event-processing:transform-error
        (test-case))

     ;; With `continue' error policy, sending the event should proceed
     ;; without the failing transformation.
      #+TODO-enable-when-fixed
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure (typep result 'event))
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.event-processing:transform-error))))))
