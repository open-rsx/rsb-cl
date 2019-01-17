;;;; model.lisp --- Tests for model classes.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(def-suite* introspection-model-root
  :in introspection-root
  :description
  "Unit test suite for the model classes.")

;;; Functions related to `process-info' classes

(test current-process-info/smoke
  "Smoke test for the `current-process-info' function."

  (let ((info (current-process-info)))
    (is (typep (process-info-process-id info)            'non-negative-integer))
    (is (typep (process-info-program-name info)          'string))
    (is (typep (process-info-commandline-arguments info) 'list))
    (is (typep (process-info-start-time info)            'local-time:timestamp))
    (is (typep (process-info-executing-user info)        'string))
    (is (typep (process-info-rsb-version info)           '(or null string)))
    (is (typep (process-info-display-name info)          '(or null string)))))

;;; Functions related to `host-info' classes

(test current-host-info/smoke
  "Smoke test for the `current-host-info' function."

  (let ((info (current-host-info)))
    (is (typep (host-info-id info)               'string))
    (is (typep (host-info-hostname info)         'string))
    (is (typep (host-info-machine-type info)     'string))
    (is (typep (host-info-machine-version info)  'string))
    (is (typep (host-info-software-type info)    'string))
    (is (typep (host-info-software-version info) 'string))))

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
