;;;; package.lisp --- Package definition cl-rsb unit tests.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:lift

   #:rsb
   #:rsb.filter)

  ;; Root test suite
  (:export
   #:root)

  ;; Test environment
  (:export
   #:spread-port)

  ;; Test utilities
  (:export
   #:check-print

   #:check-event

   #:participant-suite
   #:define-basic-participant-test-cases

   #:define-restart-method-test-case)

  (:documentation
   "This package contains unit tests for the cl-rsb system."))

(cl:in-package #:rsb.test)

(deftestsuite root ()
  ()
  (:function
   (make-id (source)
     (uuid:make-uuid-from-string source)))
  (:function
   (check-print (thing)
     (ensure
      (funcall
       (conjoin #'stringp (complement #'emptyp))
       (with-output-to-string (stream)
         (print-object thing stream))))))
  (:function
   (check-event (event scope data)
     (ensure
      (typep (event-id event) '(or null uuid:uuid)))
     (ensure-same
      (event-scope event) (make-scope scope)
      :test #'scope=)
     (ensure-same
      (event-data event) data
      :test #'equalp)))
  (:timeout 20)
  (:documentation
   "Root unit test suite of the cl-rsb system."))

(deftestsuite participant-suite ()
  ()
  (:dynamic-variables
   (*default-configuration* '(((:transport :inprocess :enabled) . "1"))))
  (:function
   (check-participant (participant scope)
     (ensure-same
      (participant-scope participant) (make-scope scope)
      :test #'scope=)
     (ensure
      (typep (participant-id participant) 'uuid:uuid))
     ;; URI stuff
     (relative-url participant)
     (abstract-uri participant)
     (let ((urls (transport-specific-urls participant)))
       (ensure (length= 1 urls)
               :report    "~@<The participant has ~D transport-specific ~
                           URLs (~{~A~^, ~}), not ~D.~@:>"
               :arguments ((length urls) urls 1)))))
  (:documentation
   "This test suite class can be used as a superclass for test suites
that test participant classes."))

(defmacro define-basic-participant-test-cases (kind &body cases)
  "Define basic test cases for the participant subclass designated by
KIND."
  (let ((suite-name (symbolicate kind "-ROOT"))
        (make-name  (symbolicate "MAKE-" kind))
        (with-name  (symbolicate "WITH-" kind)))
    `(progn
       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test constructing a ~(~A~) using `~(~A~)'."
                          kind make-name))
         construction

         (ensure-cases (uri args expected-scope)
             (list ,@cases)

           (if (eq expected-scope :error)
               (ensure-condition error
                 (apply #',make-name uri args))
               (let ((participant (apply #',make-name uri args)))
                 (unwind-protect
                      (check-participant participant expected-scope)
                   (detach/ignore-errors participant))))))

       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test `print-object' method on `~(~A~)' class."
                          kind))
         print

         (,with-name (participant ,(format nil "/~(~A~)/print" kind)
                                  ,@(when (eq kind :informer) '(t)))
           (ensure
            (not (emptyp
                  (with-output-to-string (stream)
                    (format stream "~A" participant))))))))))

(defmacro define-error-hook-test-case ((class
                                        &key
                                        (participant? t))
                                       &body body)
  "Generate a test case around BODY for the error-hook mechanism of
CLASS.

PARTICIPANT? controls whether a participant of class CLASS is created
and bound to a variable named like the value of CLASS."
  (let+ ((suite-name (format-symbol *package* "~A-ROOT" class))
         (with-macro (format-symbol :rsb "WITH-~A" class))
         (scope      (format nil "/~(~A~)/errorhook" class))
         ((&flet maybe-wrap (body)
            (if participant?
                `((,with-macro (,class ,scope) ,@body))
                body))))
    `(addtest (,suite-name
              :documentation
              ,(format nil "Test the error-hook mechanism for the `~(~A~)' class."
                       class))
       error-hook

       (let ((expected-errors)
             (seen-errors))
         (with-informer (informer ,scope t)
           ,@(maybe-wrap
              `(;; Install collecting error handler.
                (push (lambda (condition) (push condition seen-errors) (continue))
                      (hooks:hook-handlers (participant-error-hook ,class)))
                ,@body))
           ;; Make sure the expected errors match the actually seen
           ;; errors.
           (ensure-same seen-errors expected-errors :test #'equal))))))

(define-condition restart-test-error (error)
  ((method :initarg  :method
           :reader restart-test-error-method
           :documentation
           "Stores the method from which the condition was
signaled."))
  (:default-initargs
   :method (missing-required-initarg 'restart-test-error :method))
  (:report
   (lambda (condition stream)
     (format stream "~@<Simulated error in ~S method.~@:>"
             (restart-test-error-method condition))))
  (:documentation
   "This error is signaled to test establishing of restarts around
certain code."))

(defmacro define-restart-method-test-case ((class method (instance-var &rest args)
                                            &key
                                            (restarts '(log continue)))
                                           &body body)
    (let ((suite-name (symbolicate class "-ROOT"))
          (case-name  (symbolicate method "-SMOKE"))
          (var-name   (symbolicate "*" method "-FAIL?*")))
      `(progn
         ;; Define a method that signals an error unless a variable is
         ;; set.
         (declaim (special ,var-name))
         (defvar ,var-name nil)

         (defmethod ,method ((,instance-var ,class) ,@args)
           (when ,var-name (error 'restart-test-error :method ',method))
           (when (next-method-p)
             (call-next-method)))

         ;; Define the test case that invokes the method and fails if
         ;; the error is not handled by restarts.
         (addtest (,suite-name
                   :documentation
                   ,(format nil "Smoke test for the :around method on ~
                                 `~(~A~)' provided by `~(~A~)'."
                            method class))
           ,case-name

           (let ((,var-name t))
             (ensure-condition 'restart-test-error
               ,@body))

           (let+ (((&flet do-one (restart)
                     (setf ,var-name t)
                     (handler-bind
                      ((restart-test-error
                        (lambda (condition)
                          (declare (ignore condition))
                          (setf ,var-name nil)
                          (invoke-restart
                           (or (find-restart restart)
                               (error "~@<Restart ~S not found.~@:>"
                                      restart))))))
                       ,@body))))
             ,@(iter (for restart in restarts)
                     (collect `(do-one ',restart))))))))
