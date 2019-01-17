;;;; classes.lisp --- Tests for model classes.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model.test)

(def-suite rsb-model-classes-root
  :in rsb-model-root
  :description
  "Unit test suite for the model classes.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-simple-model-class-tests (class-and-options &body cases)
    (let+ (((class &key (suite-prefix '#:rsb-model-classes-))
            (ensure-list class-and-options))
           (parent-suite-name (symbolicate suite-prefix '#:root))
           (suite-name        (symbolicate suite-prefix class '#:-root))
           (construct-name    (symbolicate suite-prefix class '#:/construct))
           (print-name        (symbolicate suite-prefix class '#:/print)))
      `(progn
         (def-suite* ,suite-name
           :in ,parent-suite-name
           :description
           ,(format nil "Unit test suite for the `~(~A~)' model class."
                    class))

         (test ,construct-name
           ,(format nil "Test constructing `~(~A~)' instances." class)

           (mapc (lambda+ ((initargs &optional expected-result &ign))
                   (let+ (((&flet do-it ()
                             (apply #'make-instance ',class initargs))))
                     (case expected-result
                       (missing-required-initarg
                        (signals missing-required-initarg (do-it)))
                       (t
                        (finishes (do-it))))))
               (list ,@cases)))

         (test ,print-name
           ,(format nil "Test printing `~(~A~)' instances." class)

           (mapc (lambda+ ((initargs &optional expected-result (expected-printed t)))
                   (let+ (((&flet do-it ()
                             (princ-to-string (apply #'make-instance ',class initargs)))))
                     (cond ((eq expected-result 'missing-required-initarg))
                           ((eq expected-printed t)
                            (finishes (do-it)))
                           (t
                            (is (search expected-printed (do-it)))))))
                 (list ,@cases)))))))

;;; `participant-info' classes

(define-simple-model-class-tests participant-info
  ;; Missing required initargs.
  `((:id        ,(uuid:make-null-uuid)
     :scope     ,(make-scope "/foo")
     :type      "bar")
    missing-required-initarg)
  `((:kind      :lintener
     :scope     ,(make-scope "/foo")
     :type      "bar")
    missing-required-initarg)
  `((:kind      :lintener
     :id        ,(uuid:make-null-uuid)
     :type      "bar")
    missing-required-initarg)
  `((:kind      :lintener
     :id        ,(uuid:make-null-uuid)
     :scope     ,(make-scope "/foo"))
    missing-required-initarg)
  ;; These are OK.
  `((:kind      :listener
     :id        ,(uuid:make-null-uuid)
     :scope     ,(make-scope "/foo")
     :type      "bar")
    t "LISTENER /foo/ (T 0) 00000000")
  `((:kind      :listener
     :id        ,(uuid:make-null-uuid)
     :parent-id ,(uuid:make-null-uuid)
     :scope     ,(make-scope "/foo")
     :type      "bar")
    t "LISTENER /foo/ (T 0) 00000000")
  `((:kind       :listener
     :id         ,(uuid:make-null-uuid)
     :scope      ,(make-scope "/foo")
     :type       "bar"
     :transports ,(list (puri:uri "socket:/foo")))
    t "LISTENER /foo/ (T 1) 00000000"))

;;; `process-info' classes

(define-simple-model-class-tests process-info
  ;; Missing required initargs.
  '((:process-id 20)                 missing-required-initarg)
  '((:program-name "foo")            missing-required-initarg)
  ;; These are OK.
  `((:process-id   20
     :program-name "foo")
    t "foo[20]")
  `((:process-id   20
     :program-name "foo"
     :start-time   ,(local-time:now))
    t "foo[20]")
  '((:process-id     20
     :program-name   "foo"
     :executing-user "john")
    t "foo[20]")
  '((:process-id   20
     :program-name "foo"
     :rsb-version  "0.11")
    t "foo[20]")
  '((:process-id   20
     :program-name "foo"
     :display-name "bar")
    t "bar(foo)[20]"))

(define-simple-model-class-tests remote-process-info
  ;; Missing required initargs.
  '((:process-id 1 :program-name "foo") missing-required-initarg)
  ;; These are OK.
  '((:process-id   1
     :program-name "foo"
     :transports   ())
    t "foo[1] UNKNOWN")
  '((:process-id   20
     :program-name "foo"
     :state        :crashed
     :transports   ())
    t "foo[20] CRASHED")
  `((:process-id   20
     :program-name "foo"
     :transports   (,(puri:uri "socket://localhost:12345"))
     :state        :running
     :start-time   ,(local-time:now))
    t "foo[20] RUNNING"))

;;; `host-info' classes

(define-simple-model-class-tests host-info
  ;; Missing required initargs.
  '((:id "foo")                                             missing-required-initarg)
  '((:hostname "bar")                                       missing-required-initarg)
  ;; These are OK.
  '((:id "foo" :hostname "bar")                             t "bar ? ?")
  '((:id "foo" :hostname "bar" :machine-type "x86")         t "bar x86 ?")
  '((:id "foo" :hostname "bar" :machine-version "foo")      t "bar ? ?")
  '((:id "foo" :hostname "bar" :software-type "linux")      t "bar ? linux")
  '((:id "foo" :hostname "bar" :software-version "3.16.30") t "bar ? ?"))

(define-simple-model-class-tests remote-host-info
  ;; These are OK.
  '((:id           "foo"
     :hostname     "bar"
     :clock-offset 0.000001)
    t "bar ? ? UNKNOWN")
  '((:id           "foo"
     :hostname     "bar"
     :state        :up
     :clock-offset 0.000001)
    t "bar ? ? UP")
  '((:id           "foo"
     :hostname     "bar"
     :state        :up
     :clock-offset 0.002)
    t "bar ? ? UP +0.002 s"))

;;; Node classes

(defvar *simple-participant-info*
  (make-instance 'participant-info
                 :kind  :listener
                 :id    (uuid:make-null-uuid)
                 :scope "/foo"
                 :type  t))

(define-simple-model-class-tests basic-participant-node
  ;; Missing required initargs
  '(() missing-required-initarg)
  ;; These are OK.
  `((:info ,*simple-participant-info*)
    t "LISTENER /foo/ (T 0) (C 0) 00000000")
  `((:info     ,*simple-participant-info*
     :children ,(list *simple-participant-info*))
    t "LISTENER /foo/ (T 0) (C 1) 00000000"))

(defvar *simple-process-info*
  (make-instance 'process-info
                 :process-id   5
                 :program-name "foo"))

(defvar *remote-process-info*
  (make-instance 'remote-process-info
                 :process-id   5
                 :program-name "foo"
                 :transports   '()))

(define-simple-model-class-tests basic-process-node
  ;; Missing required initargs
  '(() missing-required-initarg)
  ;; These are OK.
  `((:info ,*simple-process-info*)
    t "foo[5] (C 0)")
  `((:info     ,*simple-process-info*
     :children ,(list *simple-participant-info*))
    t "foo[5] (C 1)")
  `((:info ,*remote-process-info*)
    t "foo[5] (C 0) UNKNOWN")
  `((:info     ,*remote-process-info*
     :children ,(list *simple-participant-info*))
    t "foo[5] (C 1) UNKNOWN"))

(defvar *simple-host-info*
  (make-instance 'host-info
                 :id       "bar"
                 :hostname "foo"))

(defvar *remote-host-info*
  (make-instance 'remote-host-info
                 :id       "bar"
                 :hostname "foo"
                 :state    :up))

(define-simple-model-class-tests basic-host-node
  ;; Missing required initargs
  '(() missing-required-initarg)
  ;; These are OK.
  `((:info ,*simple-host-info*)
    t "foo ? ? (C 0)")
  `((:info     ,*simple-host-info*
     :children ,(list *simple-process-info*))
    t "foo ? ? (C 1)")
  `((:info ,*remote-host-info*)
    t "foo ? ? (C 0) UP")
  `((:info     ,*remote-host-info*
     :children ,(list *simple-process-info*))
    t "foo ? ? (C 1) UP"))
