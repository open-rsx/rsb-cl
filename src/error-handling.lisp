;;;; error-handling.lisp --- Error handling functions used in cl-rsb.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Useful macros

(declaim (special *in-timeout?*))

(defvar *in-timeout?* nil
  "Non-nil when a timeout is active for the current thread, nil otherwise.")

(declaim (ftype (function (positive-real function) *)
                invoke-with-restart-and-timeout))

(defun invoke-with-restart-and-timeout (timeout thunk)
  "Call THUNK signaling a `bt:timeout' if it does not complete within
TIMEOUT seconds. Install a `cl:continue' restart around the
timeout/execution of THUNK which can be used to ignore errors and
timeouts.

If `*in-timeout?*' indicates that some other timeout is already active
for the current thread, install the restart but do not establish a
timeout. This is intended to prevent recursive timeouts."
  (restart-case
      ;; Give THUNK TIMEOUT seconds to complete. If it takes longer,
      ;; allow continuing via the CONTINUE restart. If another timeout
      ;; is active, call THUNK without timeout. This avoid race
      ;; conditions between timeouts and recursive timeout handling.
      (if *in-timeout?*
          (funcall thunk)
          (let ((*in-timeout?* t))
            (bt:with-timeout (timeout)
              (funcall thunk))))
    (continue ()
      :report (lambda (stream)
                (format stream "~@<Ignore the error and continue.~@:>")))))

(defmacro with-restart-and-timeout ((timeout) &body body)
  "Execute BODY signaling a `bt:timeout' if it does not complete
within TIMEOUT seconds. Install a `cl:continue' restart around the
timeout/execution of BODY which can be used to ignore errors and
timeouts.

If `*in-timeout?*' indicates that some other timeout is already active
for the current thread, install the restart but do not establish a
timeout. This is intended to prevent recursive timeouts."
  `(invoke-with-restart-and-timeout ,timeout (lambda () ,@body)))
