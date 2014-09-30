;;;; platform-common.lisp --- Utilities for platform-specific functions.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Platform information errors

(define-condition platform-information-error (simple-error
                                              chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~?~/more-conditions:maybe-print-cause/"
             (simple-condition-format-control   condition)
             (simple-condition-format-arguments condition)
             condition))))

(defun platform-information-error (cause
                                   &optional format-control
                                   &rest format-arguments)
  (error 'platform-information-error
         :format-control   format-control
         :format-arguments format-arguments
         :cause            cause))

(defun call-with-platform-information-error-translation (description thunk)
  (with-condition-translation (((error platform-information-error)
                                :format-control   "~@<Could not ~A~@:>"
                                :format-arguments (list description)))
    (funcall thunk)))

(defmacro with-platform-information-error-translation ((description) &body body)
  `(call-with-platform-information-error-translation
    ,description (lambda () ,@body)))

;;; Fallback values

(defun call-with-platform-information-fallback-values (thunk)
  (handler-bind ((platform-information-error
                  (lambda (condition)
                    (log:warn "~@<~A.~@:_Using fallback value~@:>"
                              condition)
                    (continue))))
    (funcall thunk)))

(defmacro with-platform-information-fallback-values (&body body)
  `(call-with-platform-information-fallback-values (lambda () ,@body)))

;;;

(defun current-process-start-time ()
  (let ((fallback))
    (restart-case
        (%current-process-start-time)
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Use a fallback start time value.~@:>"))
        (declare (ignore condition))
        (or fallback (setf fallback (local-time:now)))))))

(defun current-host-id ()
  (restart-case
      (%current-host-id)
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Use a fallback host id value.~@:>"))
      (declare (ignore condition))
      (machine-instance))))
