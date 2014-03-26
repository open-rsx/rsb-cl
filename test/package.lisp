;;;; package.lisp --- Package definition cl-rsb unit tests.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
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

   #:define-restart-method-test-case

   #:mock-transform/error)

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
   (check-participant (participant expected-kind scope)
     (ensure-same (participant-kind participant) expected-kind)
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

(defparameter +restart-test-scope+
  (make-scope "/rsbtest/participantcreation/restarts/error" :intern? t)
  "A special scope used in tests of participant creation restarts.")

(defparameter +restart-test-uri+
  (puri:intern-uri
   (puri:uri "inprocess:/rsbtest/participantcreation/restarts/error"))
  "A special URI used in test of participant creation restarts.")

(defmacro define-basic-participant-test-cases (class &body cases)
  "Define basic test cases for the participant subclass designated by
   CLASS."
  (let ((suite-name (symbolicate class "-ROOT"))
        (make-name  (symbolicate "MAKE-" class))
        (with-name  (symbolicate "WITH-" class))
        (kind       (make-keyword class)))
    `(progn
       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test constructing a ~(~A~) using `~(~A~)'."
                          class make-name))
         construction

         (ensure-cases (uri args expected-scope)
             (list ,@cases)

           (case expected-scope
             (error (ensure-condition error
                      (apply #',make-name uri args)))
             (t     (let ((participant (apply #',make-name uri args)))
                      (unwind-protect
                           (check-participant participant ,kind expected-scope)
                        (detach/ignore-errors participant)))))))

       (define-restart-method-test-case
           (,make-name ((scope-or-uri (eql +restart-test-scope+))
                         ,@(when (eq class 'informer) '((type t)))
                         &key  &allow-other-keys)
                        :restarts   (retry (use-scope (make-scope "/rsbtest/noerror")))
                        :suite-name ,suite-name
                        :case-name  ,(symbolicate make-name '#:/restart/scope))
         (,make-name +restart-test-scope+ ,@(when (eq class 'informer) '(t))))

       (define-restart-method-test-case
           (,make-name ((scope-or-uri (eql +restart-test-uri+))
                        ,@(when (eq class 'informer) '((type t)))
                        &key &allow-other-keys)
                        :restarts   (retry (use-uri (puri:uri "inprocess:/rsbtest/noerror")))
                        :suite-name ,suite-name
                        :case-name  ,(symbolicate make-name '#:/restart/uri))
         (,make-name +restart-test-uri+ ,@(when (eq class 'informer) '(t))))

       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test `print-object' method on `~(~A~)' class."
                          class))
         print

         (,with-name (participant ,(format nil "/~(~A~)/print" class)
                                  ,@(when (eq class 'informer) '(t)))
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
         (scope      (format nil "/rsbtest/~(~A~)/errorhook" class))
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

;;; Test utilities for restarts

(define-condition restart-test-error (error)
  ((generic-function :initarg  :generic-function
                     :reader   restart-test-error-generic-function
                     :documentation
                     "Stores the generic function from which the
                      condition was signaled."))
  (:default-initargs
   :method (missing-required-initarg
            'restart-test-error :generic-function))
  (:report
   (lambda (condition stream)
     (format stream "~@<Simulated error in ~S method.~@:>"
             (restart-test-error-generic-function condition))))
  (:documentation
   "This error is signaled to test establishing of restarts around
    certain code."))

(declaim (special *signal-error-for-restart-test?*))
(defvar *signal-error-restart-test?* nil)

(defmacro define-restart-method-test-case ((generic-function (&rest args)
                                            &key
                                            (suite-name (missing-required-argument :suite-name))
                                            (case-name  (symbolicate generic-function '#:/restart))
                                            (restarts   '(continue))
                                            (var-name   '*signal-error-for-restart-test?*))
                                           &body body)
  `(progn
     ;; Define a method that signals an error unless a variable is
     ;; set.
     (defmethod ,generic-function (,@args)
       (when ,var-name
         (error 'restart-test-error
                :generic-function ',generic-function))
       (when (next-method-p)
         (call-next-method)))

     ;; Define the test case that invokes the generic function and
     ;; fails if the error is not handled by restarts.
     (addtest (,suite-name
               :documentation
               ,(format nil "Smoke test for establishing restarts in ~
                             methods of generic-function `~(~A~)'."
                        generic-function))
       ,case-name

       (let ((,var-name t))
         (ensure-condition 'restart-test-error ,@body))

       (let+ (((&flet+ do-one ((restart &rest args))
                 (setf ,var-name t)
                 (unwind-protect
                      (handler-bind
                          ((restart-test-error
                            (lambda (condition)
                              (declare (ignore condition))
                              (setf ,var-name nil)
                              (apply #'invoke-restart
                                     (or (find-restart restart)
                                         (error "~@<Restart ~S not found.~@:>"
                                                restart))
                                     args))))
                        ,@body)
                   (setf ,var-name nil)))))
         ,@(iter (for restart in restarts)
                 (let+ (((name &rest args) (ensure-list restart)))
                   (collect `(do-one (list ',name ,@args)))))))))

;;; Utilities

(defun mock-transform/error  (event)
  (declare (ignore event))
  (error "Intentional error in transformation"))
