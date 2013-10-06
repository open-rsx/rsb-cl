;;;; variables.lisp --- Variables used in the patterns module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns)

;;; Direct call mechanism

(declaim (special *local-call*))

(defvar *local-call* nil
  "The value of this special variable can be used to determine whether
a reply is received via direct function calls within a single
thread. If so, this variable is bound to the call in question and the
result can be stored directly.")
