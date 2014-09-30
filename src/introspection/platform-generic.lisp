;;;; platform-generic.lisp --- Generic platform.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Process information

(defun current-process-id ()
  0)

(defun current-program-name-and-commandline-arguments ()
  (list* "<unknown program>" uiop:*command-line-arguments*))

(defun %current-process-start-time ()
  (with-platform-information-error-translation
      ("determine start time of the current process")
    (error "~@<Not available.~@:>")))

;;; Host information

(defun %current-host-id ()
  (with-platform-information-error-translation
      ("determine unique id of the computer")
    (error "~@<Not available.~@:>")))
