;;;; variables.lisp --- Variables used in the patterns.request-reply module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; Direct call mechanism

(defvar *local-call* nil
  "The value of this special variable can be used to determine whether
   a reply is received via direct function calls within a single
   thread. If so, this variable is bound to a value of one of the
   following forms

     t

       Indicates that a non-blocking local call is in progress.

     (FUTURE . (REQUEST . RETURN))

       Indicates that a blocking local call initiated by REQUEST is in
       progress. RETURN indicates the desired return value processing
       and FUTURE is the `future' instance for the call.

   which allow to decide whether to proceed normally, spawn a thread
   or store the result directly.")

;;; Request and reply filters

(defvar *request-filter*
  (rsb.filter:make-filter :method :method :|request|)
  "A filter which only accepts events which can be considered
   requests.")

(defvar *reply-filter*
  (rsb.filter:make-filter :method :method :|reply|)
  "A filter which only accepts events which can be considered
   replies.")
