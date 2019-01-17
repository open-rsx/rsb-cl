;;;; macros.lisp --- Unit tests for macros.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(def-suite* macros-root
  :in root
  :description
  "Unit tests for macros provided by the rsb system.")

(test (with-participant/anonymous :fixture with-configuration)
  "Test creating an anonymous participant using the `with-participant'
   macro."

  (finishes
   (with-participant (nil :listener "/rsbtest/macros-root/with-participant/anonymous"))))

;;; Test `with-participant' macro with `listener' and test
;;; `listener'-related macros

(test (with-participant/listener/smoke :fixture with-configuration)
  "Smoke test for the `with-participant' macro with a :listener
   participant."

  (with-participant (listener :listener "/rsbtest/macros-root/with-participant/listener/smoke")
    (is (typep listener 'listener))
    (check-participant listener :listener "/rsbtest/macros-root/with-participant/listener/smoke")))

(test (with-participant/listener/error-policy :fixture with-configuration)
  "Test handling of error-policy keyword parameter in
   `with-participant' macro with a :listener participant."

  (let ((payload  1)
        (calls    '())
        (received '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (listener :listener "/rsbtest/macros-root/with-listener/error-policy"
                                        :transform #'mock-transform/error
                                        ,@(when policy `(:error-policy ,policy)))
              (with-handler listener ((event) (push event received))
                (with-participant (informer :informer "/rsbtest/macros-root/with-listener/error-policy")
                  (send informer payload))))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (signals rsb.transform:transform-error (test-case))

      ;; With `continue' error policy, receiving and dispatching of
      ;; the event should proceed without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (is (typep result 'event))
        (is (= 1 (length calls)))
        (is (typep (first calls) 'rsb.transform:transform-error))
        (is (= 1 (length received)))
        (is (eql payload (event-data (first received))))))))

(test (with-handler/smoke :fixture with-configuration)
  "Smoke test for the `with-handler' macro."

  (let ((received '()))
    (with-participant (listener :listener "/rsbtest/macros-root/with-handler/smoke")
      (with-handler listener ((event) (push event received))
        (is (typep listener 'listener))
        (check-participant listener :listener "/rsbtest/macros-root/with-handler/smoke")
        (with-participant (informer :informer "/rsbtest/macros-root/with-handler/smoke")
          (send informer 1))))
    (is (= 1 (length received)))))

;;; Reader-related macros

(test (with-participant/reader/smoke :fixture with-configuration)
  "Smoke test for the `with-participant' macro with a :reader
   participant."

  (with-participant (reader :reader "/rsbtest/macros-root/with-participant/reader/smoke")
    (is (typep reader 'reader))
    (check-participant reader :reader "/rsbtest/macros-root/with-participant/reader/smoke")))

(test (with-participant/reader/error-policy :fixture with-configuration)
  "Test handling of error-policy keyword parameter in
   `with-participant' macro with a :reader participant."

  (let ((payload 1)
        (calls   '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (reader :reader "/rsbtest/macros-root/with-participant/reader/error-policy"
                                      :transform #'mock-transform/error
                                      ,@(when policy `(:error-policy ,policy)))
              (with-participant (informer :informer "/rsbtest/macros-root/with-participant/reader/error-policy")
                (send informer payload))
              (receive reader))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (signals rsb.transform:transform-error (test-case))

      ;; With `continue' error policy, sending the event should proceed
      ;; without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (is (typep result 'event))
        (is (= 1 (length calls)))
        (is (typep (first calls) 'rsb.transform:transform-error))
        (is (eql payload (event-data result)))))))

;;; Informer-related macros

(test (with-participant/informer/smoke :fixture with-configuration)
  "Smoke test for the `with-participant' macro with a :informer
   participant."

  (with-participant (informer :informer "/rsbtest/macros-root/with-participant/informer/smoke")
    (is (typep informer 'informer))
    (check-participant informer :informer "/rsbtest/macros-root/with-participant/informer/smoke")))

(test (with-participant/informer/error-policy :fixture with-configuration)
  "Test handling of error-policy keyword parameter in
   `with-participant' macro with a :informer participant."

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (informer :informer "/rsbtest/macros-root/with-informer/error-policy"
                                        :transform #'mock-transform/error
                                        ,@(when policy `(:error-policy ,policy)))
              (send informer 1))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (signals rsb.transform:transform-error (test-case))

      ;; With `continue' error policy, sending the event should proceed
      ;; without the failing transformation.
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (is (typep result 'event))
        (is (= 1 (length calls)))
        (is (typep (first calls) 'rsb.transform:transform-error))))))
