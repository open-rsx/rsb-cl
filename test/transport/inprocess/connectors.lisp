;;;; connectors.lisp --- Unit tests for the connector classes.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess.test)

;;; `in-pull-connector'

(deftestsuite in-pull-connector-root (transport-inprocess-root)
  ()
  (:documentation
   "Root unit tests suite for the `in-pull-connector' class."))

(define-basic-connector-test-cases in-pull-connector
  :initargs           '(:schema :inprocess)

  :expected-schemas   '(:inprocess)
  :expected-wire-type 't
  :expected-remote?    nil

  :expected-direction :in-pull)

;;; `in-push-connector'

(deftestsuite in-push-connector-root (transport-inprocess-root)
  ()
  (:documentation
   "Root unit tests suite for the `in-push-connector' class."))

(define-basic-connector-test-cases in-push-connector
  :initargs           '(:schema :inprocess)

  :expected-schemas   '(:inprocess)
  :expected-wire-type 't
  :expected-remote?    nil

  :expected-direction :in-push)

;;; `out-connector'

(deftestsuite out-connector-root (transport-inprocess-root)
  ()
  (:documentation
   "Root unit tests suite for the `out-connector' class."))

(define-basic-connector-test-cases out-connector
  :initargs           '(:schema :inprocess)

  :expected-schemas   '(:inprocess)
  :expected-wire-type 't
  :expected-remote?   nil

  :expected-direction :out)
