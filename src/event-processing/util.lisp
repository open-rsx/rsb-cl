;;;; util.lisp --- Utility functions used in the event-processing module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defun merge-implementation-infos (&optional info1 info2)
  (cond
    ((and (not info1) (not info2))
     :implemented)
    ((and (eq info1 :implemented) (eq info2 :implemented))
     :implemented)
    (t
     :not-implemented)))
