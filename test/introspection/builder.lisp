;;;; builder.lisp --- Tests for introspection model (un)builder.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.introspection.builder.test
  (:use
   #:cl
   #:let-plus

   #:fiveam

   #:rsb
   #:rsb.introspection)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:architecture.builder-protocol.test
   #:record-un-build-calls/peeking)

  (:import-from #:rsb.introspection
   #:timing-tracker-%clock-offset
   #:timing-tracker-%latency

   #:entry-%tracker

   #:remote-introspection-database)

  (:export
   #:rsb-introspection-builder-root

   #:run-tests)

  (:documentation
   "This package contains test for the introspection.builder module."))

(cl:in-package #:rsb.introspection.builder.test)

(def-suite* rsb-introspection-builder-root
  :description
  "Unit test suite for the introspection model (un)builder support.")

(defun run-tests ()
  (run! 'rsb-introspection-builder-root))

(defvar *stop-type* '(or number string uuid:uuid puri:uri scope))

(defun check-un-build-calls (builder atom-type cases)
  (mapc (lambda+ ((object expected-calls))
          (let+ (((&values &ign calls)
                  (record-un-build-calls/peeking
                   #'bp:walk-nodes builder atom-type object)))
            (is (equal expected-calls calls))))
        cases))

(test remote-introspection-database/smoke

  (check-un-build-calls
   t nil
   `(,(let ((database (make-instance 'remote-introspection-database)))
        `(,database ((:peek  nil () ,database)
                     (:visit nil () ,database :database ((:children . *)) ())))))))

(test process-entry/smoke

  (check-un-build-calls
   t *stop-type*
   `(,(let* ((start-time            (local-time:now))
             (commandline-arguments '("a" "b"))
             (initargs              `(:process-id     0
                                      :program-name   "foo"
                                      :start-time     ,start-time
                                      :executing-user "user"
                                      :rsb-version    "1.2.3"
                                      :display-name   "Foo"))
             (info                  (apply #'make-instance 'rsb.model:process-info
                                           :commandline-arguments commandline-arguments
                                           initargs))
             (entry                 (make-instance 'process-entry
                                                   :info     info
                                                   :receiver nil))
             (tracker               (entry-%tracker entry))
             (latency               (timing-tracker-%latency tracker)))
        `(,entry ((:peek  nil      () ,entry)
                  (:visit nil      () ,entry :process ((:latency               . 1)
                                                       (:children              . *)
                                                       (:commandline-arguments . *))
                          ,initargs)
                  (:peek  :latency () ,latency)
                  (:visit :latency () ,latency :tracked-quantity ((:history . *))
                          (:name "latency" :value nil))
                  ,@(mapcar (lambda (x) `(:peek :commandline-arguments () ,x))
                            commandline-arguments)))))))

(test host-entry/smoke

  (check-un-build-calls
   t *stop-type*
   `(,(let* ((initargs     '(:id               "1"
                             :hostname         "foo"
                             :machine-type     "mt"
                             :machine-version  "mv"
                             :software-type    "st"
                             :software-version "sv"))
             (info         (apply #'make-instance 'rsb.model:host-info
                                  initargs))
             (entry        (make-instance 'host-entry :info info))
             (tracker      (entry-%tracker entry))
             (clock-offset (timing-tracker-%clock-offset tracker))
             (latency      (timing-tracker-%latency tracker)))
        `(,entry ((:peek  nil           () ,entry)
                  (:visit nil           () ,entry :host ((:clock-offset . 1)
                                                         (:latency      . 1)
                                                         (:children     . *))
                          ,initargs)
                  (:peek  :clock-offset () ,clock-offset)
                  (:visit :clock-offset () ,clock-offset :tracked-quantity ((:history . *))
                          (:name "clock-offset" :value nil))
                  (:peek  :latency      () ,latency)
                  (:visit :latency      () ,latency :tracked-quantity ((:history . *))
                          (:name "latency" :value nil))))))))
