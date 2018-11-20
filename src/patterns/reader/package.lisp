;;;; package.lisp --- Package definition for the patterns.reader module.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.reader
  (:use
   #:cl

   #:rsb)

  ;; Reader protocol and `reader' class
  (:export
   #:receive

   #:reader)

  (:documentation
   "This package provides the `reader' participant for pull-based
    event receiving."))
