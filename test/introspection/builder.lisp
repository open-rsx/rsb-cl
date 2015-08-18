;;;; builder.lisp --- Tests for introspection model (un)builder.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.introspection.builder.test
  (:use
   #:cl
   #:let-plus

   #:lift

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
   #:rsb-introspection-builder-root)

  (:documentation
   "This package contains test for the introspection.builder module."))

(cl:in-package #:rsb.introspection.builder.test)

(deftestsuite rsb-introspection-builder-root ()
  ()
  (:documentation
   "Unit test suite for the introspection model (un)builder
    support."))

(defvar *stop-type* '(or number string uuid:uuid puri:uri scope))

(defun check-un-build-calls (builder atom-type cases)
  (mapc (lambda+ ((object expected-calls))
          (let ((calls (record-un-build-calls/peeking
                        builder atom-type object)))
            (ensure-same calls expected-calls :test #'equal)))
        cases))

(addtest (rsb-introspection-builder-root)
  remote-introspection-database/smoke

  (check-un-build-calls
   t nil
   `(,(let ((database (make-instance 'remote-introspection-database)))
        `(,database ((:peek  () ,database)
                     (:visit () ,database :database ((:children . *)) ())))))))

(addtest (rsb-introspection-builder-root)
  process-entry/smoke

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
        `(,entry ((:peek  () ,entry)
                  (:visit () ,entry :process ((:latency               . 1)
                                              (:children              . *)
                                              (:commandline-arguments . *))
                          ,initargs)
                  (:peek  () ,latency)
                  (:visit () ,latency :tracked-quantity ((:history . *))
                          (:name "latency" :value nil))
                  ,@(mapcar (lambda (x) `(:peek  () ,x))
                            commandline-arguments)))))))

(addtest (rsb-introspection-builder-root)
  host-entry/smoke

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
        `(,entry ((:peek  () ,entry)
                  (:visit () ,entry :host ((:clock-offset . 1)
                                           (:latency      . 1)
                                           (:children     . *))
                          ,initargs)
                  (:peek  () ,clock-offset)
                  (:visit () ,clock-offset :tracked-quantity ((:history . *))
                          (:name "clock-offset" :value nil))
                  (:peek  () ,latency)
                  (:visit () ,latency :tracked-quantity ((:history . *))
                          (:name "latency" :value nil))))))))
