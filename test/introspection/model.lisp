;;;; model.lisp --- Tests for model classes.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(deftestsuite introspection-model-root (introspection-root)
  ()
  (:documentation
   "Unit test suite for the model classes."))

(eval-when (:compile-toplevel :execute)
  (defmacro define-simple-model-class-tests (class &body cases)
    (let ((suite-name (symbolicate '#:introspection-model- class '#:-root)))
      `(progn
         (deftestsuite ,suite-name (introspection-model-root)
           ()
           (:documentation
            ,(format nil "Unit test suite for the `~(~A~)' model class."
                     class)))

         (addtest (,suite-name
                   :documentation
                   ,(format nil "Test constructing `~(~A~)' instances."
                            class))
           construct

           (ensure-cases (initargs &optional expected) (list ,@cases)
             (let+ (((&flet do-it ()
                       (apply #'make-instance ',class initargs))))
               (case expected
                 (error (ensure-condition error (do-it)))
                 (t     (do-it))))))

         (addtest (,suite-name
                   :documentation
                   ,(format nil "Test printing `~(~A~)' instances." class))
           print

           (ensure-cases (initargs &optional expected) (list ,@cases)
             (case expected
               ((error))
               (t (princ-to-string (apply #'make-instance ',class initargs))))))))))

;;; `participant-info' classes

(define-simple-model-class-tests participant-info
  ;; These are OK.
  `((:kind      :listener
     :id        ,(uuid:make-v1-uuid)
     :scope     ,(make-scope "/foo")
     :type      "bar"))
  `((:kind      :listener
     :id        ,(uuid:make-v1-uuid)
     :parent-id ,(uuid:make-v1-uuid)
     :scope     ,(make-scope "/foo")
     :type      "bar")))

;;; `process-info' classes

(define-simple-model-class-tests process-info
  ;; Missing required initargs.
  '((:process-id 20)                     error)
  '((:program-name "foo")                error)
  ;; These are OK.
  `((:process-id   20
     :program-name "foo"
     :start-time   ,(local-time:now))))

(addtest (introspection-model-process-info-root
          :documentation
          "Smoke test for the `current-process-info' function.")
  current-process-info/smoke

  (let ((info (current-process-info)))
    (ensure (typep (process-info-process-id info)            'non-negative-integer))
    (ensure (typep (process-info-program-name info)          'string))
    (ensure (typep (process-info-commandline-arguments info) 'list))
    (ensure (typep (process-info-start-time info)            'local-time:timestamp))))

(define-simple-model-class-tests remote-process-info
  ;; Missing required initargs.
  '((:process-id 1 :program-name "foo") error)
  ;; These are OK.
  '((:process-id   1
     :program-name "foo"
     :transports   ()))
  '((:process-id   20
     :program-name "foo"
     :state        :crashed
     :transports   ()))
  `((:process-id   20
     :program-name "foo"
     :transports   (,(puri:uri "socket://localhost:12345"))
     :state        :running
     :start-time   ,(local-time:now))))

;;; `host-info' classes

(define-simple-model-class-tests host-info
  ;; Missing required initargs.
  '((:id "foo")                 error)
  '((:hostname "bar")           error)
  ;; These are OK.
  '((:id "foo" :hostname "bar")))

(addtest (introspection-model-host-info-root
          :documentation
          "Smoke test for the `current-host-info' function.")
  current-host-info/smoke

  (let ((info (current-host-info)))
    (ensure (typep (host-info-id info)       'string))
    (ensure (typep (host-info-hostname info) 'string))))

(define-simple-model-class-tests remote-host-info
  ;; These are OK.
  '((:id           "foo"
     :hostname     "bar"
     :clock-offset 0.000001))
  '((:id           "foo"
     :hostname     "bar"
     :state        :up
     :clock-offset 0.000001))
  '((:id           "foo"
     :hostname     "bar"
     :state        :up
     :clock-offset 0.002)))

;;; `hello' and `bye' classes

(define-simple-model-class-tests hello
  ;; These are OK.
  `((:participant ,(make-instance 'participant-info
                                  :kind       :listener
                                  :id         (uuid:make-v1-uuid)
                                  :scope      (make-scope "/Bla")
                                  :type       "bla"
                                  :transports '())
     :process     ,(current-process-info)
     :host        ,(current-host-info))))

(define-simple-model-class-tests bye
  ;; Missing required initargs.
  '(()                         error)
  ;; These are OK.
  `((:id ,(uuid:make-v1-uuid))))
