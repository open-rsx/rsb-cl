;;;; participant.lisp --- Unit tests for the participant class.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(def-suite participant-root
  :in root
  :description
  "Test suite for the `participant' class.")
(in-suite participant-root)

(test participant-converter
  "Test method on `participant-converter' for the `participant'
   class."

  (mapc
   (lambda+ ((converters query error? expected))
     (let ((participant (make-instance 'participant
                                       :scope      "/"
                                       :converters converters)))
       (case expected
         (error (signals error
                  (participant-converter participant query
                                         :error? error?)))
         (t     (let ((result (participant-converter participant query
                                                     :error? error?)))
                  (is (set-equal expected result)))))))

   '((()                             number  nil ())
     (()                             number  t   error)
     (((number . :a) (integer . :b)) number  nil (:a))
     (((number . :a) (integer . :b)) integer nil (:a :b))
     (((number . :a) (integer . :b)) string  nil ())
     (((number . :a) (integer . :b)) string  t   error))))

(test error-policy-race
  "Ensure that the specified error policy is in effect when
   connector-related errors become possible."

  ;; The following code was previously racy and would sometimes signal
  ;; an error because the error signaling event handler in HANDLERS
  ;; would be executed before the `continue' error policy was
  ;; installed in the listener.
  (with-participant (informer :informer "inprocess:")
    (let ((send      (lambda () (send informer "")))
          (handlers  (list (lambda (event)
                             (declare (ignore event))
                             (error "~@<intentional error~@:>")))))
      (finishes
        (iter (repeat 1000)
              (iter (repeat 3) (bt:make-thread send))
              (with-participant
                  (listener :listener "inprocess:"
                            :error-policy #'continue
                            :handlers     handlers)
                (declare (ignore listener))))))))

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

(test failed-construction-cleanup
  "Ensure that `detach' is called when an error is signaled during
   initialization."

  ;; Make that the cleanup code in the `detach' method is executed.
  (let+ ((cleanup? nil)
         ((&flet cleanup () (setf cleanup? t))))
    (signals error
             (make-instance 'mock-cleanup-participant
                     :scope   "/participantroot/failedconstructioncleanup"
                     :cleanup #'cleanup))
    (is-true cleanup?)))
