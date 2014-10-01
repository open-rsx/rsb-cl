;;;; types.lisp --- Types in used by the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

(deftype process-state ()
  "States of a remote process."
  '(member :unknown :running :crashed))

(deftype host-state ()
  "States of a remote host."
  '(member :unknown :up :down))
