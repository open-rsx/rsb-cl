;;; conditions.lisp --- Conditions used in cl-rsb.
;;
;; Copyright (C) 2010, 2011 Jan Moringen
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

(in-package :rsb)


;;; Generic condition utilities
;;

(define-condition chainable-condition (condition)
  ((cause :initarg  :cause
	  :type     (or null condition)
	  :reader   chainable-condition-cause
	  :initform nil
	  :documentation
	  "The condition which originally caused the condition to be
signaled."))
  (:documentation
   "Instances of this class can contain another condition instance
which originally caused the condition to be signaled. This structure
can continue recursively thus forming a chain of causing
conditions."))

(defun maybe-print-cause (condition stream)
  "Print the condition that caused CONDITION to be signaled onto
STREAM."
  (when (chainable-condition-cause condition)
    (format stream "~&Caused by:~&~A"
	    (chainable-condition-cause condition))))

(defun maybe-print-explanation (condition stream)
  "Format the message contained in the `simple-condition' CONDITION on
STREAM."
  (if (simple-condition-format-control condition)
      (progn
	(format stream ":~&")
	(apply #'format stream
	       (simple-condition-format-control   condition)
	       (simple-condition-format-arguments condition)))
      (write-char #\. stream)))


;;; Program error conditions
;;

(define-condition missing-required-argument (program-error)
  ((parameter :initarg  :parameter
	      :type     symbol
	      :accessor missing-required-argument-parameter
	      :documentation
	      "The parameter for which a value should have been
supplied."))
  (:report
   (lambda (condition stream)
     (format stream "~@<No value has been supplied for the required ~
parameter ~S.~@:>"
	     (missing-required-argument-parameter condition))))
  (:documentation
   "This error is signaled when no value is supplied for a required
parameter."))

(defun missing-required-argument (parameter)
  "Signal a `missing-required-argument' error for PARAMETER."
  (error 'missing-required-argument
	 :parameter parameter))

(define-condition missing-required-initarg (missing-required-argument)
  ((class   :initarg  :class
	    :type     symbol
	    :accessor missing-required-initarg-class
	    :documentation
	    "The class which requires the missing initarg."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The initarg ~S is required by class ~S, but ~
has not been supplied.~@:>"
	     (missing-required-argument-parameter condition)
	     (missing-required-initarg-class      condition))))
  (:documentation
   "This error is signaled when an initarg that is required by a class
is not supplied."))

(defun missing-required-initarg (class initarg)
  "Signal a `missing-required-initarg' error for CLASS and INITARG."
  (error 'missing-required-initarg
	 :parameter initarg
	 :class     class))


;;; RSB-specific errors
;;

(define-condition rsb-error (error)
  ()
  (:documentation
   "This class should be mixed into all RSB-related condition
classes."))

(define-condition communication-error (rsb-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<RSB communication failed.~@:>")))
  (:documentation
   "This error is signaled when some RSB communication fails."))


;;; Participant-related errors
;;

(define-condition participation-failed (rsb-error)
  ((scope      :initarg  :scope
	       :type     scope
	       :reader   participation-failed-scope
	       :documentation
	       "The scope of the channel in which the participant
would have participated.")
   (transports :initarg  :transports
	       :type     list
	       :reader   participation-failed-transports
	       :documentation
	       "A list of transports the participant would have used
to connect to the bus."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to participate in the channel ~
designated by ~A.~@:>"
	     (participation-failed-scope condition))))
  (:documentation
   "This error is signaled when the creation of a participant (which
implies participation in a channel) fails."))

(define-condition listener-creation-failed (participation-failed)
  ()
  (:documentation
   "This error is signaled when an attempt to create a listener
fails."))

(define-condition reader-creation-failed (participation-failed)
  ()
  (:documentation
   "This error is signaled when an attempt to create a reader
fails."))

(define-condition informer-creation-failed (participation-failed)
  ((type :initarg  :type
	 :type     (or list symbol)
	 :reader   informer-creation-failed-type
	 :documentation
	 "The type of the informer for which the creation failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to create RSB informer in the channel ~
designated by ~A and type ~S.~@:>"
	     (participation-failed-scope    condition)
	     (informer-creation-failed-type condition))))
  (:documentation
   "This error is signaled when an attempt to create an informer
fails."))


;;; Event sending conditions
;;

(define-condition invalid-event (rsb-error)
  ((event    :initarg  :event
	     :reader   invalid-event-event
	     :documentation
	     "The invalid event."))
  (:default-initargs
   :event (missing-required-initarg 'invalid-event :event))
  (:report
   (lambda (condition stream)
     (format stream "~@<The event ~S was used in a context for which ~
it is not valid.~:@>"
	     (invalid-event-event    condition)
	     ;; (invalid-event-informer condition)
	     )))
  (:documentation
   "Instances of this error condition and its subclasses are signaled
when an attempt is made to use an event in a context for which it is
not valid."))

(define-condition invalid-event-type (type-error
				      invalid-event)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The type ~S of event ~S is not ~S.~@:>"
	     (event-type (type-error-datum condition))
	     (type-error-datum condition)
	     (type-error-expected-type condition))))
  (:documentation
   "This error is signaled when an event is used in a context in which
its type is unsuitable."))

(define-condition invalid-event-scope (invalid-event)
  ((expected-scope :initarg    :expected-scope
		   :type       scope
		   :reader     invalid-event-expected-scope
		   :documentation
		   "The scope the invalid event was expected to have."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The scope ~A of the event ~S is not identical ~
to the expected scope ~A.~:@>"
	     (event-scope (invalid-event-event condition))
	     (invalid-event-event condition)
	     (invalid-event-expected-scope condition))))
  (:documentation
   "This error is signaled when an event is used in a context in which
its scope is not valid."))
