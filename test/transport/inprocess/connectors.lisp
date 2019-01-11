;;;; connectors.lisp --- Unit tests for the connector classes.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess.test)

;;; `in-connector'

(deftestsuite in-connector-root (transport-inprocess-root)
  ()
  (:documentation
   "Root unit tests suite for the `in-connector' class."))

(define-basic-connector-test-cases in-connector
  :initargs           '(:schema :inprocess)

  :expected-schemas   '(:inprocess)
  :expected-wire-type 't
  :expected-remote?    nil

  :expected-direction :in)

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
