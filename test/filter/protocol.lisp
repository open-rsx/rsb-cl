;;;; protocol.lisp --- Unit tests for the filter module protocol functions.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter.test)

(def-suite protocol-root
  :in filter-root
  :description
  "Unit test suite for the protocol functions of the filter module
   protocol.")

(test filter-smoke
  "Smoke test for the `filter' function."

  ;; No such filter class.
  (signals filter-construction-error (filter :no-such-filter))
  ;; Missing initarg.
  (signals filter-construction-error (filter :origin))
  ;; Illegal initarg.
  (signals filter-construction-error (filter :origin :origin 5)))
