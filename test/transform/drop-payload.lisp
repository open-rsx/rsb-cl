;;;; drop-payload.lisp --- Unit tests for the drop-payload transform.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform.test)

(def-suite* rsb.transform.drop-payload-root
  :in rsb.transform-root
  :description
  "Unit tests for the `drop-payload' transform.")

(test drop-payload.smoke
  "Smoke test for the `drop-payload' transform."

  (call-with-transform-checking-thunk
   (lambda (do-it)
     (is (eq +dropped-payload+ (event-data (funcall do-it)))))
   :drop-payload (list "/" 5)))
