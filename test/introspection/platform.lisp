;;;; package.lisp --- Package definition for unit tests of the introspection module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(def-suite* introspection-platform-root
  :in introspection-root
  :description
  "Unit tests for the platform information functions.")

(test process-info/smoke
  "Smoke test for the process information functions."

  (is (typep (current-process-id)         'non-negative-integer))
  (is (typep (current-program-name-and-commandline-arguments)
                 'list))
  (is (typep (current-process-start-time) 'local-time:timestamp)))

(test host-info/smoke
  "Smoke test for the host information functions."

  (is (typep (current-host-id) 'string)))
