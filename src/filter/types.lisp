;;;; types.lisp --- Types used in the filter module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(deftype fallback-policy ()
  "Designators for fallback filter behaviors."
  '(member :match :do-not-match))
