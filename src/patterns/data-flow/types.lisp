;;;; types.lisp --- Types used by the patterns.data-flow module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

(deftype suspension-state ()
  "TODO"
  '(member :suspended :running))
