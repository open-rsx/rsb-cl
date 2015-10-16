;;;; drop-payload.lisp --- Unit tests for the drop-payload transform.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform.test)

(deftestsuite rsb.transform.drop-payload-root (rsb.transform-root)
  ()
  (:documentation
   "Unit tests for the `drop-payload' transform."))

(addtest (rsb.transform.drop-payload-root
          :documentation
          "Smoke test for the `drop-payload' transform.")
  smoke

  (call-with-transform-checking-thunk
   (lambda (do-it)
     (ensure-same (event-data (funcall do-it)) +dropped-payload+))
   :drop-payload (list "/" 5)))
