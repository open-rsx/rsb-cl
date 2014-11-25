;;;; drop-payload.lisp --- Unit tests for the drop-payload transform.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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

  (let ((transform (make-transform :drop-payload))
        (event     (make-event "/" 5)))
    (ensure-same (event-data (transform! transform event))
                 +dropped-payload+)))
