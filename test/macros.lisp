;;;; macros.lisp --- Unit tests for macros.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.test)

(deftestsuite macros-root (root
			   participant-suite)
  ()
  (:documentation
   "Unit tests for macros provided by the cl-rsb system."))

(addtest (macros-root
          :documentation
	  "Smoke test for the `with-listener' macro.")
  with-listener-smoke

  (with-listener (listener "/listener")
    (ensure (typep listener 'listener))
    (check-participant listener "/listener")))

(addtest (macros-root
          :documentation
	  "Smoke test for the `with-reader' macro.")
  with-reader-smoke

  (with-reader (reader "/reader")
    (ensure (typep reader 'reader))
    (check-participant reader "/reader")))

(addtest (macros-root
          :documentation
	  "Smoke test for the `with-informer' macro.")
  with-informer-smoke

  (with-informer (informer "/informer" t)
    (ensure (typep informer 'informer))
    (check-participant informer "/informer")))
