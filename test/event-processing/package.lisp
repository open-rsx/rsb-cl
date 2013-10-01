;;;; package.lisp --- Package definition for unit tests of the event-processing module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsb.event-processing.test
  (:use
   :cl
   :alexandria
   :let-plus
   :lift

   :rsb
   :rsb.event-processing

   :rsb.test)

  (:documentation
   "This package contains unit tests for the event-processing
module"))

(cl:in-package :rsb.event-processing.test)

(deftestsuite event-processing-root (root)
  ()
  (:documentation
   "Root unit test suite for the event-processing module."))

;;; `mock-processor' mock class

(defclass mock-processor ()
  ((handled :initarg  :handled
            :type     list
            :accessor processor-handled
            :initform nil)))

(defmethod handle ((sink mock-processor) (data t))
  (appendf (processor-handled sink) (list data)))
