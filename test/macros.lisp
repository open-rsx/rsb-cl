;;;; macros.lisp --- Unit tests for macros.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite macros-root (root
                           participant-suite)
  ()
  (:documentation
   "Unit tests for macros provided by the rsb system."))

(addtest (macros-root
          :documentation
          "Test creating an anonymous participant using the
           `with-participant' macro.")
  with-participant/anonymous

  (with-participant (nil :listener "/rsbtest/macros-root/with-participant/anonymous")))

;;; Test `with-participant' macro with `listener' and test
;;; `listener'-related macros

(addtest (macros-root
          :documentation
          "Smoke test for the `with-participant' macro with
           a :listener participant.")
  with-participant/listener/smoke

  (with-participant (listener :listener "/rsbtest/macros-root/with-participant/listener/smoke")
    (ensure (typep listener 'listener))
    (check-participant listener :listener "/rsbtest/macros-root/with-participant/listener/smoke")))

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-participant' macro with a :listener participant.")
  with-participant/listener/error-policy

  (let ((calls    '())
        (received '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (listener :listener "/rsbtest/macros-root/with-listener/error-policy"
                                        :transform #'mock-transform/error
                                        ,@(when policy `(:error-policy ,policy)))
              (with-handler listener ((event) (push event received))
                (with-participant (informer :informer "/rsbtest/macros-root/with-listener/error-policy")
                  (send informer 1))))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition rsb.transform:transform-error
        (test-case))

      ;; With `continue' error policy, receiving and dispatching of
      ;; the event should proceed without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure (typep result 'event))
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.transform:transform-error))
        (ensure-same (length received) 1)))))

(addtest (macros-root
          :documentation
          "Smoke test for the `with-handler' macro.")
  with-handler/smoke

  (let ((received '()))
    (with-participant (listener :listener "/rsbtest/macros-root/with-handler/smoke")
      (with-handler listener ((event) (push event received))
        (ensure (typep listener 'listener))
        (check-participant listener :listener "/rsbtest/macros-root/with-handler/smoke")
        (with-participant (informer :informer "/rsbtest/macros-root/with-handler/smoke")
          (send informer 1))))
    (ensure-same (length received) 1)))

;;; Reader-related macros

(addtest (macros-root
          :documentation
          "Smoke test for the `with-participant' macro with a :reader
           participant.")
  with-participant/reader/smoke

  (with-participant (reader :reader "/rsbtest/macros-root/with-participant/reader/smoke")
    (ensure (typep reader 'reader))
    (check-participant reader :reader "/rsbtest/macros-root/with-participant/reader/smoke")))

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-participant' macro with a :reader participant.")
  with-participant/reader/error-policy

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (reader :reader "/rsbtest/macros-root/with-participant/reader/error-policy"
                                      :transform #'mock-transform/error
                                      ,@(when policy `(:error-policy ,policy)))
              (with-participant (informer :informer "/rsbtest/macros-root/with-participant/reader/error-policy")
                (send informer 1))
              (receive reader))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition rsb.transform:transform-error
        (test-case))

      ;; With `continue' error policy, sending the event should proceed
      ;; without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure (typep result 'event))
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.transform:transform-error))))))

;;; Informer-related macros

(addtest (macros-root
          :documentation
          "Smoke test for the `with-participant' macro with
           a :informer participant.")
  with-participant/informer/smoke

  (with-participant (informer :informer "/rsbtest/macros-root/with-participant/informer/smoke")
    (ensure (typep informer 'informer))
    (check-participant informer :informer "/rsbtest/macros-root/with-participant/informer/smoke")))

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-participant' macro with a :informer participant.")
  with-participant/informer/error-policy

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (informer :informer "/rsbtest/macros-root/with-informer/error-policy"
                                        :transform #'mock-transform/error
                                        ,@(when policy `(:error-policy ,policy)))
              (send informer 1))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition rsb.transform:transform-error
        (test-case))

      ;; With `continue' error policy, sending the event should proceed
      ;; without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure (typep result 'event))
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.transform:transform-error))))))
