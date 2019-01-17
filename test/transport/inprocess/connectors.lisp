;;;; connectors.lisp --- Unit tests for the connector classes.
;;;;
;;;; Copyright (C) 2011-2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess.test)

;;; `in-pull-connector'

(def-suite* in-pull-connector-root
  :in transport-inprocess-root
  :description
  "Root unit tests suite for the `in-pull-connector' class.")

(define-basic-connector-test-cases in-pull-connector
  :initargs           '(:schema :inprocess)

  :expected-schemas   '(:inprocess)
  :expected-wire-type 't
  :expected-remote?    nil

  :expected-direction :in-pull)

;;; `in-push-connector'

(def-suite* in-push-connector-root
  :in transport-inprocess-root
  :description
  "Root unit tests suite for the `in-push-connector' class.")

(define-basic-connector-test-cases in-push-connector
  :initargs           '(:schema :inprocess)

  :expected-schemas   '(:inprocess)
  :expected-wire-type 't
  :expected-remote?    nil

  :expected-direction :in-push)

;;; `out-connector'

(def-suite* out-connector-root
  :in transport-inprocess-root
  :description
  "Root unit tests suite for the `out-connector' class.")

(define-basic-connector-test-cases out-connector
  :initargs           '(:schema :inprocess)

  :expected-schemas   '(:inprocess)
  :expected-wire-type 't
  :expected-remote?   nil

  :expected-direction :out)
