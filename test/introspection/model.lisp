;;;; model.lisp --- Tests for model classes.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(deftestsuite introspection-model-root (introspection-root)
  ()
  (:documentation
   "Unit test suite for the model classes."))

;;; Functions related to `process-info' classes

(addtest (introspection-model-root
          :documentation
          "Smoke test for the `current-process-info' function.")
  current-process-info/smoke

  (let ((info (current-process-info)))
    (ensure (typep (process-info-process-id info)            'non-negative-integer))
    (ensure (typep (process-info-program-name info)          'string))
    (ensure (typep (process-info-commandline-arguments info) 'list))
    (ensure (typep (process-info-start-time info)            'local-time:timestamp))
    (ensure (typep (process-info-executing-user info)        'string))
    (ensure (typep (process-info-rsb-version info)           '(or null string)))
    (ensure (typep (process-info-display-name info)          '(or null string)))))

;;; Functions related to `host-info' classes

(addtest (introspection-model-root
          :documentation
          "Smoke test for the `current-host-info' function.")
  current-host-info/smoke

  (let ((info (current-host-info)))
    (ensure (typep (host-info-id info)               'string))
    (ensure (typep (host-info-hostname info)         'string))
    (ensure (typep (host-info-machine-type info)     'string))
    (ensure (typep (host-info-machine-version info)  'string))
    (ensure (typep (host-info-software-type info)    'string))
    (ensure (typep (host-info-software-version info) 'string))))

;;; `hello' and `bye' classes

(rsb.model.test::define-simple-model-class-tests (hello :suite-prefix #:introspection-model-)
  ;; These are OK.
  `((:participant ,(make-instance 'participant-info
                                  :kind       :listener
                                  :id         (uuid:make-v1-uuid)
                                  :scope      (make-scope "/Bla")
                                  :type       "bla"
                                  :transports '())
     :process     ,(current-process-info)
     :host        ,(current-host-info))))

(rsb.model.test::define-simple-model-class-tests (bye :suite-prefix #:introspection-model-)
  ;; Missing required initargs.
  '(()                         missing-required-initarg)
  ;; These are OK.
  `((:id ,(uuid:make-v1-uuid))))
