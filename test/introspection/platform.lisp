;;;; package.lisp --- Package definition for unit tests of the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(deftestsuite introspection-platform-root (introspection-root)
  ()
  (:documentation
   "Unit tests for the platform information functions."))

(addtest (introspection-platform-root
          :documentation
          "Smoke test for the process information functions.")
  process-info/smoke

  (ensure (typep (current-process-id)         'non-negative-integer))
  (ensure (typep (current-program-name-and-commandline-arguments)
                 'list))
  (ensure (typep (current-process-start-time) 'local-time:timestamp)))

(addtest (introspection-platform-root
          :documentation
          "Smoke test for the host information functions.")
  host-info/smoke

  (ensure (typep (current-host-id) 'string)))
