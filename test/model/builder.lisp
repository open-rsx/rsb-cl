;;;; builder.lisp --- Tests for model (un)builder.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.model.builder.test
  (:use
   #:cl
   #:let-plus

   #:lift

   #:rsb
   #:rsb.model)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:rsb.model
   #:basic-participant-node
   #:basic-process-node
   #:basic-host-node)

  (:import-from #:architecture.builder-protocol.test
   #:record-un-build-calls/peeking)

  (:export
   #:rsb-model-builder-root)

  (:documentation
   "This package contains test for the model.builder module."))

(cl:in-package #:rsb.model.builder.test)

(deftestsuite rsb-model-builder-root ()
  ()
  (:documentation
   "Unit test suite for the model (un)builder support."))

(defvar *stop-type* '(or number string uuid:uuid puri:uri scope))

(defun check-un-build-calls (builder atom-type cases)
  (mapc (lambda+ ((object expected-calls))
          (let+ (((&values &ign calls)
                  (record-un-build-calls/peeking
                   #'bp:walk-nodes builder atom-type object)))
            (ensure-same calls expected-calls :test #'equal)))
        cases))

(let* ((id            (uuid:make-null-uuid))
       (scope         (make-scope "/" :intern? t))
       (transport-uri (puri:uri "socket://foo"))
       (initargs      `(:kind       :listener
                                    :id         ,id
                                    :scope      ,scope
                                    :type       t)))

  (addtest (rsb-model-builder-root)
    participant-info/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let ((info (apply #'make-instance 'participant-info
                           :transports (list transport-uri)
                           initargs)))
          `(,info ((:peek  nil         () ,info)
                   (:visit nil         () ,info :participant ((:transports . *)) ,initargs)
                   (:peek  :transports () ,transport-uri)))))))

  (addtest (rsb-model-builder-root)
    participant-node/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let* ((info (apply #'make-instance 'participant-info
                            :transports (list transport-uri)
                            initargs))
               (node (make-instance 'basic-participant-node :info info)))
          `(,info ((:peek  nil         () ,info)
                   (:visit nil         () ,info :participant ((:transports . *)) ,initargs)
                   (:peek  :transports () ,transport-uri))))))))

(let* ((start-time            (local-time:now))
       (commandline-arguments '("a" "b"))
       (transport             "socket://foo")
       (initargs              `(:process-id     0
                                :program-name   "foo"
                                :start-time     ,start-time
                                :executing-user "user"
                                :rsb-version    "1.2.3"
                                :display-name   "Foo")))

  (addtest (rsb-model-builder-root)
    process-info/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let* (
               (info       (apply #'make-instance 'process-info
                                  :commandline-arguments commandline-arguments
                                  initargs)))
          `(,info ((:peek  nil () ,info)
                   (:visit nil () ,info :process ((:commandline-arguments . *))
                           ,initargs)
                   ,@(mapcar (lambda (x) `(:peek :commandline-arguments () ,x))
                             commandline-arguments)))))))

  (addtest (rsb-model-builder-root)
    remote-process-info/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let* ((info (apply #'make-instance 'remote-process-info
                            :commandline-arguments commandline-arguments
                            :state                 :running
                            :transports            (list transport)
                            initargs)))
          `(,info ((:peek  nil         () ,info)
                   (:visit nil         () ,info :process ((:transports            . *)
                                                          (:commandline-arguments . *))
                           ( :state :running ,@initargs))
                   (:peek  :transports () ,transport)
                   ,@(mapcar (lambda (x) `(:peek :commandline-arguments () ,x))
                             commandline-arguments)))))))

  (addtest (rsb-model-builder-root)
    process-node/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let* ((info (apply #'make-instance 'process-info
                            :commandline-arguments commandline-arguments
                            initargs))
               (node (make-instance 'basic-process-node :info info)))
          `(,node ((:peek  nil () ,node)
                   (:visit nil () ,node :process ((:children              . *)
                                                  (:commandline-arguments . *))
                           ,initargs)
                   ,@(mapcar (lambda (x) `(:peek :commandline-arguments () ,x))
                             commandline-arguments))))))))

(let* ((initargs '(:id               "1"
                   :hostname         "foo"
                   :machine-type     "mt"
                   :machine-version  "mv"
                   :software-type    "st"
                   :software-version "sv")))

  (addtest (rsb-model-builder-root)
    host-info/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let ((info (apply #'make-instance 'host-info initargs)))
          `(,info ((:peek  nil () ,info)
                   (:visit nil () ,info :host () ,initargs)))))))

  (addtest (rsb-model-builder-root)
    remote-host-info/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let ((info (apply #'make-instance 'remote-host-info
                           :state :up initargs)))
          `(,info ((:peek  nil () ,info)
                   (:visit nil () ,info :host () (:state :up ,@initargs))))))))

  (addtest (rsb-model-builder-root)
    host-node/smoke

    (check-un-build-calls
     t *stop-type*
     `(,(let* ((info (apply #'make-instance 'host-info initargs))
               (node (make-instance 'basic-host-node :info info)))
          `(,info ((:peek  nil () ,info)
                   (:visit nil () ,info :host () ,initargs))))))))
