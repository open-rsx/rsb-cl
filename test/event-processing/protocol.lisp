;;;; protocol.lisp --- Test for the protocol functions provided by the event-processing module.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

(defclass mock-access-processor ()
  ((access :initarg :access
           :reader  processor-access)))

(defmethod access? ((processor mock-access-processor)
                    (part      symbol)
                    (mode      symbol))
  (assoc-value (processor-access processor) (cons part mode) :test #'equal))

(addtest (event-processing-root
          :documentation
          "Smoke test for the `access?' generic function.")
  access?/smoke

  (let* ((processor1 (make-instance 'mock-access-processor
                                    :access '(((:data . :read) . t))))
         (processor2 (make-instance 'mock-access-processor
                                    :access '(((:scope . :read) . t))))
         (processors (list processor1 processor2)))
    (ensure-same (access? processor1 :data           :read)  t)
    (ensure-same (access? processor1 :data           :write) nil)
    (ensure-same (access? processor1 :scope          :read)  nil)
    (ensure-same (access? processor1 :meta-data      :read)  nil)

    (ensure-same (access? processor1 '(:data)        :read)  t)
    (ensure-same (access? processor1 '(:scope)       :read)  nil)
    (ensure-same (access? processor1 '(:data :scope) :read)  t)

    (ensure-same (access? processors :data           :read)  t)
    (ensure-same (access? processors :scope          :read)  t)
    (ensure-same (access? processors :meta-data      :read)  nil)))
