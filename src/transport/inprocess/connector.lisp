;;;; connector.lisp --- Superclass for inprocess connector classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.inprocess)


;;;
;;

(defvar *by-scope* (make-hash-table :test     #'equal
				    ;:weakness :value
				    )
  "Association of scopes to event sinks interested in the respective
scopes.")

(defun by-scope (scope)
  "Return a list of connectors that are associated to SCOPE."
  (let ((key (%scope->key scope)))
    (gethash key *by-scope*)))

(defun (setf by-scope) (new-value scope)
  "Set the of handlers associated to SCOPE to NEW-VALUE."
  (let ((key (%scope->key scope)))
    (setf (gethash key *by-scope*) new-value)))


;;;
;;

(defclass connector (rsb.transport:connector)
  ()
  (:metaclass connector-class)
  (:default-initargs
   :schema :inprocess
   :host   (load-time-value (machine-instance) t)
   :port   (load-time-value (sb-posix:getpid) t))
  (:wire-type t) ;; The Lisp process is the medium, so t (any Lisp
		 ;; object) should be a reasonable wire-type
  (:schemas   :inprocess)
  (:documentation
   "Superclass for connector classes of the inprocess transport."))


;;; Utility functions
;;

(defun %scope->key (scope)
  "Convert the URI object URI into a scope string. "
  (scope-string scope))
