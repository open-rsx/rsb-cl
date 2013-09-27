;;;; out-connector.lisp --- Unit tests for the inprocess:out-connector class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.inprocess.test)

(deftestsuite out-connector-root (transport-inprocess-root
				      connector-suite)
  ()
  (:documentation
   "Root unit tests suite for the `out-connector' class."))

(define-basic-connector-test-cases out-connector
    :expected-direction :out
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))
