;;;; in-push-connector.lisp --- Unit tests for the inprocess:in-push-connector class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.inprocess.test)

(deftestsuite in-push-connector-root (transport-inprocess-root
                                      connector-suite)
  ()
  (:documentation
   "Root unit tests suite for the `in-push-connector' class."))

(define-basic-connector-test-cases in-push-connector
    :expected-direction :in-push
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))
