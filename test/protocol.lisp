;;;; protocol.lisp --- Unit tests for the protocol of the cl-rsb system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

;;; Tests for the `make-participant' generic function

(deftestsuite make-participant-root (root)
  ()
  (:documentation
   "Unit tests for the `participant-hook' generic function."))

(defclass nested-mock-participant.inner (participant) ())
(rsb::register-participant-class 'nested-mock-participant.inner)
(defmethod initialize-instance :after ((instance nested-mock-participant.inner)
                                       &key
                                       scope
                                       signal-participant-creation-error?
                                       &allow-other-keys)
  (if signal-participant-creation-error?
      (error 'participant-creation-error
             :kind       :nested-mock-participant.inner
             :scope      scope
             :transports '())
      (error "intentional error")))

(defclass nested-mock-participant.outer (participant) ())
(rsb::register-participant-class 'nested-mock-participant.outer)
(defmethod initialize-instance :after ((instance nested-mock-participant.outer)
                                       &key &allow-other-keys)
  (make-participant :nested-mock-participant.inner "/"))

(defun participant-creation-error-cause-by-nested-errors? (condition)
  (flet ((check-one (condition kind)
           (and (typep condition 'participant-creation-error)
                (eq (participant-creation-error-kind condition) kind))))
    (and (check-one condition :nested-mock-participant.outer)
         (check-one (cause condition) :nested-mock-participant.inner)
         (typep (cause (cause condition)) 'simple-error))))
(deftype participant-creation-error-cause-by-nested-errors ()
  '(satisfies participant-creation-error-cause-by-nested-errors?))

(addtest (make-participant-root
          :documentation
          "Test chaining of `participant-creation-error' conditions in
           case of recursive `make-participant' calls.")
  error-from-nested-call

  (flet ((test-case (scope &rest initargs &key &allow-other-keys)
           (ensure-condition 'participant-creation-error-cause-by-nested-errors
             (apply #'make-participant :nested-mock-participant.outer scope
                    initargs))))
    (test-case "/")
    (test-case (puri:uri "/"))
    (test-case (make-scope "/"))

    (test-case "/"              :signal-participant-creation-error? t)
    (test-case (puri:uri "/")   :signal-participant-creation-error? t)
    (test-case (make-scope "/") :signal-participant-creation-error? t)))

;;; Hook tests

(deftestsuite hooks-root (root)
  ()
  (:documentation
   "Unit tests for `*make-participant-hook*' and
    `*participant-state-change-hook*'."))

(defun call-with-hook-call-tracking (hook thunk
                                     &key
                                     handler)
  (let ((hook-calls '()))
    (hooks:with-handlers
        ((hook (lambda (&rest args)
                 (appendf hook-calls (list args))
                 (if handler
                     (apply handler args)
                     (values))
                 (first args))))
      (funcall thunk))
    hook-calls))

(defmacro with-hook-call-tracking ((hook &optional calls-place) &body body)
  (let+ (((&flet make-call ()
            `(call-with-hook-call-tracking ',hook (lambda () ,@body)))))
    (if calls-place
        `(setf ,calls-place ,(make-call))
        (make-call))))

(addtest (hooks-root
          :documentation
          "Smoke test for `*make-participant-hook*'.")
  make-participant-hook/smoke

  (ensure-cases ((kind scope &rest args) expected)
      '(((:reader        "inprocess:/rsbtest/make-participant-hook/smoke")
         (:converters :transports :transform))
        ((:listener      "inprocess:/rsbtest/make-participant-hook/smoke")
         (:converters :transports :transform))
        ((:informer      "inprocess:/rsbtest/make-participant-hook/smoke" :type t)
         (:converters :transports :transform :type))

        ((:local-server  "inprocess:/rsbtest/make-participant-hook/smoke")
         (:converters :transports :transform))
        ((:remote-server "inprocess:/rsbtest/make-participant-hook/smoke")
         (:converters :transports :transform)))

    (let ((participant))
      (ensure-same
       (with-hook-call-tracking (*make-participant-hook*)
         (with-participant (participant1 (apply #'make-participant
                                                kind scope args))
           (setf participant participant1)))
       (list (list participant expected))
       :test (lambda (calls expected)
               (every
                (lambda+ ((call-participant     call-initargs)
                          (expected-participant expected-initargs))
                  (ensure-same call-participant expected-participant)
                  (ensure-same
                   (iter (for (key _) :on call-initargs :by #'cddr) (collect key))
                   expected-initargs
                   :test #'set-equal))
                calls expected))))))

(define-condition buggy-handler-error (error) ())

(defun participant-creation-error-caused-by-buggy-handler-error? (thing)
  (and (typep thing 'participant-creation-error)
       (typep (cause thing) 'buggy-handler-error)))

(deftype participant-creation-error-caused-by-buggy-handler-error ()
  '(satisfies participant-creation-error-caused-by-buggy-handler-error?))

(addtest (hooks-root
          :documentation
          "Test signaled error in case of a buggy handler in
           `*make-participant-hook*'.")
  make-participant-hook/buggy-handler

  (let+ ((scope "/rsbtest/make-participant-hook/buggy-handler")
         (uri   (concatenate 'string "inprocess:" scope))
         ((&flet do-it (&optional thunk)
            (hooks:with-handlers (('*make-participant-hook*
                                   (lambda (&rest args)
                                     (declare (ignore args))
                                     (error 'buggy-handler-error))))
              (with-participant (participant (make-participant :listener uri))
                (when thunk (funcall thunk participant)))))))

    ;; Without invoking restarts, the condition should be wrapped in a
    ;; `participant-creation-error'.
    (ensure-condition
        'participant-creation-error-caused-by-buggy-handler-error
      (do-it))

    ;; Skip the buggy handler by using the `continue' restart. Make
    ;; sure we get the requested participant instance in this case.
    (handler-bind ((error #'continue))
      (do-it (rcurry #'check-participant :listener scope)))))

(addtest (hooks-root
          :documentation
          "Smoke test for `*participant-state-change-hook*'")
  participant-state-change-hook/smoke

  (ensure-cases (kind scope &rest args)
      '((:reader        "inprocess:/rsbtest/participant-state-change-hook/smoke")
        (:listener      "inprocess:/rsbtest/participant-state-change-hook/smoke")
        (:informer      "inprocess:/rsbtest/participant-state-change-hook/smoke"
                        :type t)

        (:local-server  "inprocess:/rsbtest/participant-state-change-hook/smoke")
        (:remote-server "inprocess:/rsbtest/participant-state-change-hook/smoke"))

    (let ((participant))
      (ensure-same
       (with-hook-call-tracking (*participant-state-change-hook*)
         (with-participant (participant1 (apply #'make-participant
                                                kind scope args))
           (setf participant participant1)))
       (list (list participant :detached))
       :test #'equal))))
