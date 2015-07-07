;;;; types.lisp --- Types in used by the model module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model)

(deftype process-state ()
  "States of a remote process."
  '(member :unknown :running :crashed))

(deftype host-state ()
  "States of a remote host."
  '(member :unknown :up :down))
