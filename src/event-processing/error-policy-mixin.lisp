;;; error-policy-mixin.lisp --- A mixin for client-supplied error policies.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.event-processing)

(defclass error-policy-mixin ()
  ((error-policy :initarg  :error-policy
		 :type     (or null function)
		 :accessor processor-error-policy
		 :initform nil
		 :documentation
		 "Stores the error policy that should be applied in
case of errors. Nil or a function to be called in case of dispatch
errors. Functions will be called with the condition object is sole
argument.
Functions installed here should be prepared to be called from multiple
threads simultaneously."))
  (:documentation
   "This class is intended to be mixed into classes that need to
handle conditions according to a client-supplied policy."))

(defmethod apply-error-policy ((processor error-policy-mixin)
			       (condition t))
  (if-let ((policy (processor-error-policy processor)))
    (funcall policy condition)
    (log1 :warn processor "Do not have a error handling policy installed; unwinding")))

(defun invoke-with-error-policy (processor thunk)
  "Invoke THUNK with a handler that applies the error policy of
PROCESSOR."
  (handler-bind
      ((error (curry #'apply-error-policy processor)))
    (funcall thunk)))

(defmacro with-error-policy ((processor) &body body)
  "Execute BODY with a condition handler that applies the error policy
of processor."
  `(invoke-with-error-policy ,processor #'(lambda () ,@body)))
