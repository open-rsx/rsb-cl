;;;; protocol.lisp --- Unit tests for the protocol of the cl-rsb system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

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

(defun participant-creation-error-caused-by-program-error? (thing)
  (and (typep thing 'participant-creation-error)
       (typep (cause thing) 'program-error)))

(deftype participant-creation-error-caused-by-program-error ()
  '(satisfies participant-creation-error-caused-by-program-error?))

(addtest (hooks-root
          :documentation
          "Test signaled error in case of a buggy handler in
           `*make-participant-hook*'.")
  make-participant-hook/buggy-handler

  (ensure-condition 'participant-creation-error-caused-by-program-error
    (hooks:with-handlers (('*make-participant-hook*
                           (lambda (&rest args)
                            (declare (ignore args))
                            nil)))
      (with-participant (participant (make-participant :listener "inprocess:/rsbtest/make-participant-hook/buffer-handler"))
        (declare (ignore participant))))))

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
