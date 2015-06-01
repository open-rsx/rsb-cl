;;;; connectors.lisp --- Unit tests for the connector classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.inprocess.test)

;;; `in-pull-connector'

(deftestsuite in-pull-connector-root (transport-inprocess-root)
  ()
  (:documentation
   "Root unit tests suite for the `in-pull-connector' class."))

(define-basic-connector-test-cases in-pull-connector
    :expected-direction :in-pull
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))

;;; `in-push-connector'

(deftestsuite in-push-connector-root (transport-inprocess-root)
  ()
  (:documentation
   "Root unit tests suite for the `in-push-connector' class."))

(define-basic-connector-test-cases in-push-connector
    :expected-direction :in-push
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))

;;; `out-connector'

(deftestsuite out-connector-root (transport-inprocess-root)
  ()
  (:documentation
   "Root unit tests suite for the `out-connector' class."))

(define-basic-connector-test-cases out-connector
    :expected-direction :out
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))
