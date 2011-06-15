;;; error-policy-mixin.lisp --- A mixin for dispatch error handling.
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
		 "Nil or a function to be called in case of dispatch
errors. Functions installed here should be prepared to be called from
multiple threads simultaneously."))
  (:documentation
   "This mixin class is intended to mixed into processor classes that
perform potentially error signaling tasks in their `dispatch'
methods. This class adds an :around method on `dispatch' that installs
restarts for error recovery and optionally calls an error policy
function."))

(defmethod dispatch :around ((processor error-policy-mixin)
			     (event     event))
  "Install log and ignore restarts around a call to the next
`dispatch' method. In case of an error, call the error-policy function
of PROCESSOR, if any."
  (handler-bind
      ((error #'(lambda (condition)
		  (bind (((:accessors-r/o
			   (policy processor-error-policy)) processor))
		    (unless policy
		      (log1 :info "~@<Processor ~A does not have a ~
error handling policy installed; unwinding.~@:>"
			    processor))
		    (when policy
		      (funcall policy condition))))))
    (restart-case
	(call-next-method)
      (log (&optional condition)
	:report (lambda (stream)
		  (format stream "~@<Log a message and ignore the ~
failure to dispatch event ~A.~@:>"
			  event))
	(log1 :warn "~@<Event processor ~A failed to dispatch the event ~
~A~@[: ~A~].~@:>" processor event condition)
	nil)
      (ignore ()
	:report (lambda (stream)
		  (format stream "~@<Ignore the failure to dispatch ~
event ~A.~@:>"
			  event))
	nil))))
