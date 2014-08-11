;;;; processor-mixins.lisp --- Unit tests for processor mixin classes.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

;;; `error-policy-mixin' test suite

(deftestsuite error-policy-mixin-root (event-processing-root)
  ((simple-processor (make-instance 'error-policy-mixin)))
  (:documentation
   "Test suite for the `error-policy-mixin' class."))

(defun error-policy-mixin.signaling-function ()
  "A function that unconditionally signals an error."
  (restart-case
      (error "~@<I like to signal.~@:>")
    (continue (&optional condition)
      (declare (ignore condition))
      nil)))

(macrolet
    ((define-smoke-test (name &body call-form)
       `(addtest (error-policy-mixin-root
                  :documentation
                  "Test basic error handling policies of the
                   `error-policy-mixin' class.")
          ,name

          ;; Error policy nil means to just unwind.
          (setf (processor-error-policy simple-processor) nil)
          (ensure-condition 'simple-error
            ,@call-form)

          ;; The error policy #'continue should prevent the error from
          ;; being signaled.
          (setf (processor-error-policy simple-processor) #'continue)
          ,@call-form)))

  (define-smoke-test smoke/function
      (call-with-error-policy
       simple-processor #'error-policy-mixin.signaling-function))

  (define-smoke-test smoke/macro
      (with-error-policy (simple-processor)
        (error-policy-mixin.signaling-function))))

;;; {error-policy,restart}-{dispatcher,handler}-mixin test suites

(defun error-policy+restart-mixins.signaling-handler (event)
  "A handler that unconditionally signals an error."
  (error "~@<I hate ~A.~@:>" event))

(macrolet
    ((define-error-policy+restart-mixins-tests (method name)
       (let* ((error-policy-class-name
               (symbolicate '#:error-policy- name '#:-mixin))
              (error-policy-suite-name
               (symbolicate error-policy-class-name '#:-root))
              (restart-class-name
               (symbolicate '#:restart- name '#:-mixin))
              (restart-suite-name
               (symbolicate restart-class-name '#:-root)))
         `(progn
            (deftestsuite ,error-policy-suite-name (event-processing-root)
              ()
              (:documentation
               ,(format nil "Test suite for the `~(~A~)' class."
                        error-policy-class-name)))

            (addtest (,error-policy-suite-name
                      :documentation
                      ,(format nil "Smoke test for the `~(~A~)' class."
                               error-policy-class-name))
              smoke

              (let ((processor (make-instance (ensure-processor-class
                                               '(error-policy-mixin
                                                 ,error-policy-class-name
                                                 broadcast-processor)))))
                (push #'error-policy+restart-mixins.signaling-handler
                      (handlers processor))

                ;; Error policy nil means to just unwind.
                (setf (processor-error-policy processor) nil)
                (ensure-condition 'simple-error
                  (handle processor (make-event "/" "bla")))

                ;; The error policy #'continue should prevent the error from being
                ;; signaled.
                (setf (processor-error-policy processor) #'continue)
                (ensure-same
                 (restart-case
                     (handle processor (make-event "/" "bla"))
                   (continue (&optional condition)
                     :continue-restart))
                 :continue-restart)))

            (deftestsuite ,restart-suite-name (event-processing-root)
              ()
              (:documentation
               ,(format nil "Test suite for the `~(~A~)' class."
                        restart-class-name)))

            (addtest (,restart-suite-name
                      :documentation
                      ,(format nil "Smoke test for the `~(~A~)' class."
                               restart-class-name))
              smoke

              (let ((processor (make-instance (ensure-processor-class
                                               '(,restart-class-name
                                                 broadcast-processor)))))
                (handler-bind
                    ((error
                      (lambda (condition)
                        (let ((restart (find-restart 'continue)))
                          (ensure restart)
                          (ensure-same
                           ,(format nil "Ignore the failures to ~A" method)
                           (princ-to-string restart)
                           :test #'search)))))
                  (handle processor (make-event "/" "bla")))))))))

  (define-error-policy+restart-mixins-tests dispatch dispatcher)
  (define-error-policy+restart-mixins-tests handle   handler))

;;; `transform-mixin' tests

(deftestsuite rsb.event-processing.transform-mixin-root (event-processing-root)
  ()
  (:documentation
   "Unit tests for the `transform-mixin' processor mixin class.

See test suite for `transform!' generic function."))

(defclass transform-mock-processor (transform-mixin
                                    mock-processor)
  ())

(addtest (rsb.event-processing.transform-mixin-root
          :documentation
          "Smoke test for the `transform-mixin' processor mixin
class.")
  smoke

  (ensure-cases (initargs objects expected)
      `(;; Some invalid transforms.
        ((:transform :no-such-transform)     (:does-not-matter) transform-error)
        ((:transform (:no-such-transform))   (:does-not-matter) transform-error)
        ((:transform ,#'1+)                  (:wrong-type)      transform-error)

        ;; These are valid
        ((:transform ,#'1+)                  (1 2 3)            (2 3 4))
        ((:transform (,#'1+ ,(curry #'* 2))) (1 2 3)            (3 5 7))
        ((:transform (,(curry #'* 2) ,#'1+)) (1 2 3)            (4 6 8)))

    (let+ (((&flet do-it ()
              (let ((processor (apply #'make-instance
                                      'transform-mock-processor initargs)))
                (mapc (curry #'handle processor) objects)
                (processor-handled processor)))))
      (case expected
        (transform-error (ensure-condition 'transform-error (do-it)))
        (t               (ensure-same (do-it) expected :test #'equal))))))
