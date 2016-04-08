;;;; package.lisp --- Package definition for unit tests of the introspection module.
;;;;
;;;; Copyright (C) 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(def-suite introspection-platform-root
  :in introspection-root
  :description
  "Unit tests for the platform information functions.")
(in-suite introspection-platform-root)

(test process-info/smoke
  "Smoke test for the process information functions."

  (ensure (typep (current-process-id)         'non-negative-integer))
  (ensure (typep (current-program-name-and-commandline-arguments)
                 'list))
  (ensure (typep (current-process-start-time) 'local-time:timestamp)))

(test host-info/smoke
  "Smoke test for the host information functions."

  (ensure (typep (current-host-id) 'string)))
