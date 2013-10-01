;;;; protocol.lisp --- Unit tests for the filter module protocol functions.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.filter.test)

(deftestsuite protocol-root (filter-root)
  ()
  (:documentation
   "Unit test suite for the protocol functions of the filter module
protocol."))

(addtest (protocol-root
          :documentation
          "Smoke test for the `filter' function.")
  filter-smoke

  ;; No such filter class.
  (ensure-condition 'filter-construction-error
    (filter :no-such-filter))
  ;; Missing initarg.
  (ensure-condition 'filter-construction-error
    (filter :origin))
  ;; Illegal initarg.
  (ensure-condition 'filter-construction-error
    (filter :origin :origin 5)))
