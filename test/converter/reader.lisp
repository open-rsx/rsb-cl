;;;; reader.lisp --- Unit tests for the read/print-based converter.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter.test)

(def-suite reader-root
  :in converter-root
  :description
  "Unit tests for the read/print-based converter.")
(in-suite reader-root)

(define-basic-converter-test-cases (:reader)
    `(("\"bla\"" string  "bla")
      ("5"       integer 5)
      ("5"       string  :error)))
