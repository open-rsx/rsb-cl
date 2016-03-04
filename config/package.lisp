;;;; package.lisp --- Package definition for the config module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb-config
  (:use
   #:cl)

  (:export
   #:copy-file)

  (:export
   #:project-version
   #:update-version-file))
