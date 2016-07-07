;;;; package.lisp --- Package definition cl-rsb unit tests.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

  (:import-from #:rsb
   #:+root-scope+)

  (:shadow
   #:random-element)

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

   #:call-with-access-checking
   #:with-access-checking
   #:without-access-checking

   #:access-checking-event
   #:event-readable?
   #:event-writable?

   #:access-rules-for-processor
   #:add-access-checking-to-event
   #:make-access-checking-event
   #:make-access-checking-event-for-processor

   #:participant-suite
   #:define-basic-participant-test-cases

   #:define-restart-test-method
   #:define-restart-test-case

   #:mock-transform/error

   #:mock-participant-state

   #:*simple-parent*)

  ;; Random testing utilities
  (:export
   #:random-boolean
   #:random-integer
   ; #:random-element TODO later
   #:random-sequence
   #:random-character
   #:random-string
   #:random-list

   #:random-scope-component
   #:random-scope

   #:random-kind)

  (:documentation
   "This package contains unit tests for the cl-rsb system."))

(cl:in-package #:rsb.test)

(defparameter +test-configuration+
  '(((:transport :inprocess :enabled) . t)
    ((:introspection :enabled)        . nil)))

(deftestsuite root ()
  ()
  (:dynamic-variables
   (*configuration* +test-configuration+))
  (:function
   (make-id (source)
     (uuid:make-uuid-from-string source)))
  (:timeout 20)
  (:documentation
   "Root unit test suite of the cl-rsb system."))

;;; Tests Utilities

(defun check-print (thing)
  (ensure (funcall (conjoin #'stringp (complement #'emptyp))
                   (with-output-to-string (stream)
                     (print-object thing stream)))))

(defun check-event (event scope data)
  (ensure (typep (event-id event) '(or null uuid:uuid)))
  (ensure-same (event-scope event) (make-scope scope) :test #'scope=)
  (ensure-same (event-data event) data :test #'equalp))

;;; Tools related to events

(defgeneric (setf rsb.event-processing:access?)
    (new-value processor part mode)
  (:documentation
   "Set MODE access to PART for PROCESSOR to NEW-VALUE.

    Only used by test utilities."))

(defvar *check-access?* nil)

(defun call-with-access-checking (enabled thunk)
  (let ((*check-access?* enabled))
    (funcall thunk)))

(defmacro with-access-checking (() &body forms)
  "Executing forms with event access checking enabled."
  `(call-with-access-checking t (lambda () ,@forms)))

(defmacro without-access-checking (() &body forms)
  "Executing forms without event access checking enabled."
  `(call-with-access-checking nil (lambda () ,@forms)))

(defclass access-checking-event (event)
  ((rules :initarg  :rules
          :accessor event-rules
          :initform '()
          :documentation
          "Stores rules of the form

             (PART MODE ALLOWED?)

           where PART names an event part, MODE is an access mode such
           as :read or :write and ALLOWED? is a Boolean indicating
           whether the access should be allowed."))
  (:documentation
   "Instances are specialized events that only permit access to their
    slots as specified by rules.

    The rules work as a whitelist: allow accesses are forbidden unless
    specifically allowed."))

(defmethod print-object :around ((object access-checking-event)
                                 stream)
  ;; Allow all accesses while printing OBJECT.
  (without-access-checking ()
    (call-next-method)))

(defmethod rsb.ep:access? ((processor access-checking-event)
                           (part      t)
                           (mode      t))
  (loop :for (rule-part rule-mode allowed?) :in (event-rules processor) :do
     (when (and (eq rule-part part) (eq rule-mode mode) allowed?)
       (return-from rsb.ep:access? t))))

(defmethod (setf rsb.ep:access?) ((new-value t)
                                  (processor access-checking-event)
                                  (part      t)
                                  (mode      t))
  (push (list part mode new-value) (event-rules processor)))

(defgeneric check-access (event part mode)
  (:method ((event access-checking-event) (part t) (mode t))
    (when *check-access?*
      (unless (rsb.ep:access? event part mode)
        (error "~@<~S access to ~S of ~A should not be performed.~@:>"
               mode part event)))))

;; Define :before methods on the event accessor generic function that
;; check whether the attempted access is allowed and signal an error
;; if it is not.
(macrolet ((define-checked-method (name lambda-list part modes)
             `(progn
                ,@(when (member :read modes)
                    `((defmethod ,name :before
                        ((event access-checking-event)
                         ,@lambda-list)
                        (check-access event ,part :read))))

                ,@(when (member :write modes)
                    `((defmethod (setf ,name) :before
                        ((new-value t)
                         (event     access-checking-event)
                         ,@lambda-list)
                        (check-access event ,part :write))))))
           (define-checked-methods ()
             `(progn
                ,@(mappend
                   (lambda+ ((part . (&key accessors &allow-other-keys)))
                     (mapcar
                      (lambda+ ((name lambda-list modes))
                        `(define-checked-method ,name ,lambda-list ,part ,modes))
                      accessors))
                   rsb.ep:*event-parts*))))
  (define-checked-methods))

(defun access-rules-for-processor (processor)
  (loop :named :outer :for (part . plist) :in rsb.ep:*event-parts*
     :append
     (loop :for mode :in '(:read :write)
        :collect
        `(,part ,mode ,(rsb.ep:access? processor part mode)))))

(defun add-access-checking-to-event (event rules)
  (change-class event 'access-checking-event
                :create-timestamp? nil
                :rules             rules))

(defun make-access-checking-event (scope data
                                   &rest args &key
                                   rules
                                   &allow-other-keys)
  "Construction helper for `access-checking-event'."
  (add-access-checking-to-event
   (apply #'make-event scope data
          (remove-from-plist args :rules))
   rules))

(defun make-access-checking-event-for-processor
    (processor scope data
     &rest args &key origin sequence-number &allow-other-keys)
  "Return an event based on SCOPE, DATA and ARGS that only permits
   read/write access to its parts as declared necessary by PROCESSOR."
  (let ((event (apply #'make-access-checking-event scope data
                      :rules (access-rules-for-processor processor)
                      (remove-from-plist args :origin :sequence-number))))
    (when origin          (setf (event-origin          event) origin))
    (when sequence-number (setf (event-sequence-number event) sequence-number))
    event))

;;; Tools related to participants

(deftestsuite participant-suite ()
  ()
  (:documentation
   "This test suite class can be used as a superclass for test suites
    that test participant classes."))

(defun check-participant (participant expected-kind scope
                          &key
                          (check-transport-urls? t))
  (ensure-same (participant-kind participant) expected-kind)
  (ensure-same (participant-scope participant) (make-scope scope)
               :test #'scope=)
  (ensure (typep (participant-id participant) 'uuid:uuid))
  ;; URI stuff
  (relative-url participant)
  (abstract-uri participant)
  (when check-transport-urls?
    (let ((urls (transport-specific-urls participant)))
      (ensure (length= 1 urls)
              :report    "~@<The participant has ~D transport-specific ~
                          URLs (~{~A~^, ~}), not ~D.~@:>"
              :arguments ((length urls) urls 1)))))

(defmacro define-basic-participant-test-cases (class-and-options &body cases)
  "Define basic test cases for the participant subclass designated by
   CLASS."
  (let+ (((class &key (check-transport-urls? t))
          (ensure-list class-and-options))
         (suite-name (symbolicate class "-ROOT"))
         (kind       (make-keyword class)))
    `(progn
       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test constructing a ~(~A~) using `~(~A~)'."
                          class 'make-participant))
         construction/make-participant

         (ensure-cases (uri initargs expected-scope)
             (list ,@cases)

           (let+ (((&flet do-it () (apply #'make-participant ,kind uri
                                          initargs))))
             (case expected-scope
               (error (ensure-condition error
                        (detach/ignore-errors (do-it))))
               (t     (with-active-participant (participant (do-it))
                        (check-participant
                         participant ,kind expected-scope
                         :check-transport-urls? ,check-transport-urls?)))))))

       (define-restart-test-case
           (make-participant-restart/scope
            :suite-name ,suite-name
            :restarts   (retry (use-scope (make-scope "/rsbtest/noerror")))
            :condition  participant-creation-error-caused-by-restart-test-error)
         (detach/ignore-errors
          (make-participant ,kind +restart-test-scope+)))

       (define-restart-test-case
           (make-participant-restart/uri
            :suite-name ,suite-name
            :restarts   (retry (use-uri (puri:uri "inprocess:/rsbtest/noerror")))
            :condition  participant-creation-error-caused-by-restart-test-error)
         (detach/ignore-errors
          (make-participant ,kind +restart-test-uri+)))

       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test `print-object' method on `~(~A~)' class."
                          class))
         print

         (with-participant (participant
                            ,kind ,(format nil "/~(~A~)/print" class))
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
         (kind       (make-keyword class))
         (scope      (format nil "/rsbtest/~(~A~)/errorhook" class))
         ((&flet maybe-wrap (body)
            (if participant?
                `((with-participant (,class ,kind ,scope) ,@body))
                body))))
    `(addtest (,suite-name
              :documentation
              ,(format nil "Test the error-hook mechanism for the `~(~A~)' class."
                       class))
       error-hook

       (let ((expected-errors)
             (seen-errors))
         (with-participant (informer :informer ,scope)
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
  ((context :initarg  :context
            :reader   restart-test-error-context
            :documentation
            "Stores the context from which the condition was
             signaled."))
  (:default-initargs
   :context (missing-required-initarg
             'restart-test-error :context))
  (:report
   (lambda (condition stream)
     (format stream "~@<Simulated error in ~S.~@:>"
             (restart-test-error-context condition))))
  (:documentation
   "This error is signaled to test establishing of restarts around
    certain code."))

(defvar *signal-error-for-restart-test?* nil)

(defmacro define-restart-test-method (name (&rest args)
                                      &key
                                      (var-name '*signal-error-for-restart-test?*))
  "Define a method on the generic function named by NAME that signals
   a `restart-test-error' when the variable named by VAR-NAME is
   true."
  `(defmethod ,name (,@args)
     (when ,var-name
       (error 'restart-test-error
              :context '(generic-function ,name)))
     (when (next-method-p)
       (call-next-method))))

(defmacro define-restart-test-case
    ((name
      &key
      (suite-name (missing-required-argument :suite-name))
      (restarts   '(continue))
      (var-name   '*signal-error-for-restart-test?*)
      (condition  'restart-test-error))
     &body body)
  "Define the test case NAME in suite SUITE-NAME that executes BODY
   and fails if the condition of type CONDITION is not handled by the
   restarts listed in RESTARTS."
  `(addtest (,suite-name
             :documentation

             ,(format nil "Smoke test for establishing ~{~S~^, ~} ~
                           restart~P."
                      (mapcar (compose #'first #'ensure-list) restarts)
                      (length restarts)))
     ,name

     (let+ (((&flet do-it ()
               ,@body))
            ((&flet do-one-restart (restart &rest args)
               (let ((,var-name t))
                 (handler-bind
                     ((,condition
                       (lambda (condition)
                         (declare (ignore condition))
                         (setf ,var-name nil)
                         (apply #'invoke-restart
                                (or (find-restart restart)
                                    (error "~@<Restart ~S not found.~@:>"
                                           restart))
                                args))))
                   (do-it))))))
       ;; Make sure CONDITION is signaled when no restarts are
       ;; invoked.
       (let ((,var-name t))
         (ensure-condition ',condition (do-it)))

       ;; For each restart in RESTARTS, make sure that invoking it
       ;; works properly.
       ,@(iter (for restart in restarts)
               (let+ (((name &rest args) (ensure-list restart)))
                 (collect `(do-one-restart ',name ,@args)))))))

;;; `make-participant' restarts

(defun participant-creation-error-caused-by-restart-test-error? (condition)
  (and (typep condition         'participant-creation-error)
       (typep (cause condition) 'restart-test-error)))

(deftype participant-creation-error-caused-by-restart-test-error ()
  '(satisfies participant-creation-error-caused-by-restart-test-error?))

(defparameter +restart-test-scope+
  (make-scope "/rsbtest/participantcreation/restarts/error" :intern? t)
  "A special scope used in tests of participant creation restarts.")

(define-restart-test-method make-participant
    ((kind t) (scope-or-uri (eql +restart-test-scope+)) &key))

(defparameter +restart-test-uri+
  (puri:intern-uri
   (puri:uri "inprocess:/rsbtest/participantcreation/restarts/error"))
  "A special URI used in test of participant creation restarts.")

(define-restart-test-method make-participant
    ((kind t) (scope-or-uri (eql +restart-test-uri+)) &key))

;;; Utilities

(defun mock-transform/error (event)
  (declare (ignore event))
  (error "Intentional error in transformation"))

(defclass mock-participant (participant)
  ((state :initform :attached
          :accessor mock-participant-state)))

(rsb::register-participant-class 'mock-participant :mock)

(defmethod participant-kind ((participant mock-participant))
  :mock)

(defmethod detach ((participant mock-participant))
  (setf (mock-participant-state participant) :detached))

(defvar *simple-parent*
  (let ((*configuration* +test-configuration+))
    (make-participant :mock "/rsbtest/simple-parent")))
