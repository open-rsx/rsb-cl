;;;; participant.lisp --- Unit tests for the participant class.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite participant-root (root)
  ()
  (:documentation
   "Test suite for the `participant' class."))

(addtest (participant-root
          :documentation
          "Test method on `participant-converter' for the
           `participant' class.")
  participant-converter

  (ensure-cases (converters query error? expected)
      '((()                             number  nil ())
        (()                             number  t   :error)
        (((number . :a) (integer . :b)) number  nil (:a))
        (((number . :a) (integer . :b)) integer nil (:a :b))
        (((number . :a) (integer . :b)) string  nil ())
        (((number . :a) (integer . :b)) string  t   :error))

    (let ((participant (make-instance 'participant
                                      :scope      "/"
                                      :converters converters)))
      (if (eq expected :error)
          (ensure-condition 'error
            (participant-converter participant query
                                   :error? error?))
          (let ((result (participant-converter participant query
                                               :error? error?)))
            (ensure-same result expected
                         :test #'set-equal))))))

(addtest (participant-root
          :documentation
          "Ensure that the specified error policy is in effect when
           connector-related errors become possible.")
  error-policy-race

  ;; The following code was previously racy and would sometimes signal
  ;; an error because the error signaling event handler in HANDLERS
  ;; would be executed before the `continue' error policy was
  ;; installed in the listener.
  (with-participant (informer :informer "inprocess:")
    (let ((send      (lambda () (send informer "")))
          (handlers  (list (lambda (event)
                             (declare (ignore event))
                             (error "~@<intentional error~@:>")))))
      (iter (repeat 1000)
            (iter (repeat 3) (bt:make-thread send))
            (with-participant
                (listener :listener "inprocess:"
                          :error-policy #'continue
                          :handlers     handlers)
              (declare (ignore listener)))))))

(defclass mock-cleanup-participant (participant)
  ((cleanup :initarg  :cleanup
            :reader   cleanup)))

(defmethod shared-initialize :after ((instance   mock-cleanup-participant)
                                     (slot-names t)
                                     &key)
  (error "something went wrong"))

(defmethod detach ((participant mock-cleanup-participant))
  (funcall (cleanup participant))
  (error "another thing went wrong"))

(addtest (participant-root
          :documentation
          "Ensure that `detach' is called when an error is signaled
           during initialization.")
  failed-construction-cleanup

  ;; Make that the cleanup code in the `detach' method is executed.
  (let+ ((cleanup? nil)
         ((&flet cleanup () (setf cleanup? t))))
    (ensure-condition 'error
      (make-instance 'mock-cleanup-participant
                     :scope   "/participantroot/failedconstructioncleanup"
                     :cleanup #'cleanup))
    (ensure cleanup?)))
