;;;; processor-mixins.lisp --- Unit tests for processor mixin classes.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing.test)

;;; `error-policy-mixin' test suite

(def-suite* error-policy-mixin-root
  :in event-processing-root
  :description
  "Test suite for the `error-policy-mixin' class.")

(defun error-policy-mixin.signaling-function (&optional recursive?)
  "A function that unconditionally signals an error."
  (restart-case
      (error "~@<I like to signal.~@:>")
    (continue (&optional condition)
      (declare (ignore condition))
      (when recursive?
        (with-simple-restart (continue "Nested Continue")
          (error "~@<Another error.~@:>")))
      nil)))

(macrolet
    ((define-smoke-test (name &body call-form)
       `(test ,name
          "Test basic error handling policies of the
           `error-policy-mixin' class."

          (let ((processor (make-instance 'error-policy-mixin)))
            ;; Error policy nil means to just unwind.
            (setf (processor-error-policy processor) nil)
            (signals simple-error ,@call-form)

            ;; The error policy #'continue should prevent the error
            ;; from being signaled.
            (setf (processor-error-policy processor) #'continue)
            ,@call-form))))

  (define-smoke-test error-policy-mixin/smoke/function
    (call-with-error-policy
     processor #'error-policy-mixin.signaling-function))

  (define-smoke-test error-policy-mixin/smoke/macro
    (with-error-policy (processor)
      (error-policy-mixin.signaling-function)))

  (define-smoke-test error-policy-mixin/recursive-error
    (with-error-policy (processor)
      (error-policy-mixin.signaling-function t))))

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
              (error-policy-smoke-test-name
                (symbolicate error-policy-class-name '#:/smoke))
              (restart-class-name
                (symbolicate '#:restart- name '#:-mixin))
              (restart-suite-name
                (symbolicate restart-class-name '#:-root))
              (restart-smoke-test-name
                (symbolicate restart-class-name '#:/smoke)))
         `(progn
            (def-suite* ,error-policy-suite-name
              :in event-processing-root
              :description
              ,(format nil "Test suite for the `~(~A~)' class."
                       error-policy-class-name))

            (test ,error-policy-smoke-test-name
              ,(format nil "Smoke test for the `~(~A~)' class."
                       error-policy-class-name)

              (let ((processor (make-instance (ensure-processor-class
                                               '(error-policy-mixin
                                                 ,error-policy-class-name
                                                 broadcast-processor)))))
                (push #'error-policy+restart-mixins.signaling-handler
                      (handlers processor))

                ;; Error policy nil means to just unwind.
                (setf (processor-error-policy processor) nil)
                (signals simple-error
                  (handle processor (make-event "/" "bla")))

                ;; The error policy #'continue should prevent the error from being
                ;; signaled.
                (setf (processor-error-policy processor) #'continue)
                (is (eq :continue-restart
                        (restart-case
                            (handle processor (make-event "/" "bla"))
                          (continue (&optional condition)
                            (declare (ignore condition))
                            :continue-restart))))))

            (def-suite* ,restart-suite-name
              :in event-processing-root
              :description
              ,(format nil "Test suite for the `~(~A~)' class."
                       restart-class-name))

            (test ,restart-smoke-test-name
              ,(format nil "Smoke test for the `~(~A~)' class."
                       restart-class-name)

              (let ((processor (make-instance (ensure-processor-class
                                               '(,restart-class-name
                                                 broadcast-processor)))))
                (handler-bind
                    ((error
                      (lambda (condition)
                        (declare (ignore condition))
                        (let ((restart (find-restart 'continue)))
                          (is (not (null restart)))
                          (is (search
                               ,(format nil "Ignore the failures to ~A" method)
                               (princ-to-string restart)))))))
                  (handle processor (make-event "/" "bla")))))))))

  (define-error-policy+restart-mixins-tests dispatch dispatcher)
  (define-error-policy+restart-mixins-tests handle   handler))

;;; `transform-mixin' tests

(def-suite* rsb.event-processing.transform-mixin-root
  :in event-processing-root
  :description
  "Unit tests for the `transform-mixin' processor mixin class.

   See test suite for `transform!' generic function.")

(defclass transform-mock-processor (transform-mixin
                                    mock-processor)
  ())

(test transform-mixin/smoke
  "Smoke test for the `transform-mixin' processor mixin class."

  (mapc
   (lambda+ ((initargs objects expected))
     (let+ (((&flet do-it ()
               (let ((processor (apply #'make-instance
                                       'transform-mock-processor initargs)))
                 (mapc (curry #'handle processor) objects)
                 (processor-handled processor)))))
       (case expected
         (rsb.transform:transform-error
          (signals rsb.transform:transform-error (do-it)))
         (t
          (is (equal expected (do-it)))))))

   `( ;; Some invalid transforms.
     ((:transform :no-such-transform)     (:does-not-matter) rsb.transform:transform-error)
     ((:transform (:no-such-transform))   (:does-not-matter) rsb.transform:transform-error)
     ((:transform ,#'1+)                  (:wrong-type)      rsb.transform:transform-error)

     ;; These are valid
     ((:transform ,#'1+)                  (1 2 3)            (2 3 4))
     ((:transform (,#'1+ ,(curry #'* 2))) (1 2 3)            (3 5 7))
     ((:transform (,(curry #'* 2) ,#'1+)) (1 2 3)            (4 6 8)))))

;;; `sink-dispatcher-mixin'

(def-suite* rsb.event-processing.sink-dispatcher-mixin-root
  :in event-processing-root
  :description
  "Unit test for the `sink-dispatcher-mixin' processor mixin class.")

(test sink-dispatcher-mixin/smoke
  "Smoke test for the `sink-dispatcher-mixin' processor mixin class."

  (let+ ((dispatcher (make-instance 'sink-dispatcher-mixin))
         ((&flet subscribe (subject scope)
            (notify dispatcher subject (subscribed (make-scope scope)))))
         ((&flet unsubscribe (subject scope)
            (notify dispatcher subject (unsubscribed (make-scope scope)))))
         ((&flet dispatch (scope event)
            (dispatch dispatcher (scope-and-event
                                  (make-scope scope) event)))))
    ;; Simple scenario with two sinks.
    (let* ((events1  '())
           (handler1 (lambda (event) (push event events1)))
           (events2  '())
           (handler2 (lambda (event) (push event events2))))
      (subscribe handler1 "/foo")
      (subscribe handler2 "/foo/bar")

      (dispatch "/" 1)
      (is (equal events1 '()))
      (is (equal events2 '()))

      (dispatch "/foo" 2)
      (is (equal events1 '(2)))
      (is (equal events2 '()))

      (dispatch "/foo/bar" 3)
      (is (equal events1 '(3 2)))
      (is (equal events2 '(3)))

      (unsubscribe handler1 "/foo")

      (dispatch "/foo/bar" 4)
      (is (equal events1 '(3 2)))
      (is (equal events2 '(4 3))))))
