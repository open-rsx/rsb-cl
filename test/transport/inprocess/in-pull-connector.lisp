;;;; in-pull-connector.lisp --- Unit tests for the inprocess:in-pull-connector class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.inprocess.test)

(deftestsuite in-pull-connector-root (transport-inprocess-root
				      connector-suite)
  ()
  (:documentation
   "Root unit tests suite for the `in-pull-connector' class."))

(define-basic-connector-test-cases in-pull-connector
    :expected-direction :in-pull
    :expected-wire-type 't
    :expected-schemas   '(:inprocess))
