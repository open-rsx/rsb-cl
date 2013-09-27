;;;; error-handling-dispatcher-mixin.lisp --- A mixin for dispatch error handling.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.event-processing)

(defclass error-handling-dispatcher-mixin (error-policy-mixin)
  ()
  (:documentation
   "This mixin class is intended to mixed into processor classes that
perform potentially error signaling tasks in their `dispatch'
methods. This class adds an :around method on `dispatch' that installs
restarts for error recovery and optionally calls an error policy
function."))

(defmethod dispatch :around ((processor error-handling-dispatcher-mixin)
			     (event     event))
  "Install log and ignore restarts around a call to the next
`dispatch' method. In case of an error, call the error-policy function
of PROCESSOR, if any."
  (with-error-policy (processor)
    (restart-case
	(call-next-method)
      (log (&optional condition)
	:report (lambda (stream)
		  (format stream "~@<Log a message and ignore the ~
failure to dispatch event ~A.~@:>"
			  event))
	(log1 :warn processor "Failed to dispatch the event ~A~@[: ~A~]" event condition)
	nil)
      (continue ()
	:report (lambda (stream)
		  (format stream "~@<Ignore the failure to dispatch ~
event ~A.~@:>"
			  event))
	nil))))
