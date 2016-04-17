;;;; util.lisp --- Utilities used in the filter module.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defun ensure-uuid (thing)
  (etypecase thing
    (uuid:uuid            thing)
    (string               (uuid:make-uuid-from-string thing))
    (nibbles:octet-vector (uuid:byte-array-to-uuid thing))))
