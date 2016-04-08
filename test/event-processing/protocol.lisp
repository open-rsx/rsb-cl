;;;; protocol.lisp --- Test for the protocol functions provided by the event-processing module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

(in-suite event-processing-root)

(defclass mock-access-processor ()
  ((access :initarg :access
           :reader  processor-access)))

(defmethod access? ((processor mock-access-processor)
                    (part      symbol)
                    (mode      symbol))
  (assoc-value (processor-access processor) (cons part mode) :test #'equal))

(test access?/smoke
  "Smoke test for the `access?' generic function."

  (let* ((processor1 (make-instance 'mock-access-processor
                                    :access '(((:data . :read) . t))))
         (processor2 (make-instance 'mock-access-processor
                                    :access '(((:scope . :read) . t))))
         (processors (list processor1 processor2)))
    (is (eq t   (access? processor1 :data           :read)))
    (is (eq nil (access? processor1 :data           :write)))
    (is (eq nil (access? processor1 :scope          :read)))
    (is (eq nil (access? processor1 :meta-data      :read)))

    (is (eq t   (access? processor1 '(:data)        :read)))
    (is (eq nil (access? processor1 '(:scope)       :read)))
    (is (eq t   (access? processor1 '(:data :scope) :read)))

    (is (eq t   (access? processors :data           :read)))
    (is (eq t   (access? processors :scope          :read)))
    (is (eq nil (access? processors :meta-data      :read)))))
