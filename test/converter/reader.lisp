;;;; reader.lisp --- Unit tests for the read/print-based converter.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter.test)

(deftestsuite reader-root (converter-root)
  ()
  (:documentation
   "Unit tests for the read/print-based converter."))

(define-basic-converter-test-cases (:reader)
    `(("\"bla\"" string  "bla")
      ("5"       integer 5)
      ("5"       string  :error)))
