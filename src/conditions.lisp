;;; conditions.lisp --- Conditions used in cl-rsb.
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb)


;;; RSB-specific errors
;;

(define-condition rsb-error (error)
  ()
  (:documentation
   "This class should be mixed into all RSB-related condition
classes."))

(define-condition communication-error (rsb-error
				       reference-condition)
  ()
  (:default-initargs
   :references (list (documentation-ref/rsb-manual "Troubleshooting")
		     (documentation-ref/rsb-bug)))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<RSB communication failed.~@:>")))
  (:documentation
   "This error is signaled when some RSB communication fails."))


;;; Participant-related errors
;;

(define-condition participation-failed (rsb-error
					chainable-condition
					reference-condition)
  ((scope      :initarg  :scope
	       :type     (or puri:uri scope)
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
  (:default-initargs
   :references (list (documentation-ref/rsb-manual "Troubleshooting")
		     (documentation-ref/rsb-bug)))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to participate in the channel ~
designated by ~
~S~/rsb::maybe-print-transport-configuration/~/more-conditions::maybe-print-cause/~@:>"
	     (participation-failed-scope      condition)
	     (participation-failed-transports condition)
	     condition)))
  (:documentation
   "This error is signaled when the creation of a participant (which
implies participation in a channel) fails."))

(defun maybe-print-transport-configuration (stream transports &optional colon? at?)
  "Print the transport configuration TRANSPORTS to STREAM."
  (declare (ignore colon? at?))
  (format stream "~@[ using transport configuration~{~{~_+ ~@(~A~) ~
transport with options~&~2T~@<~@;~@{~16S~^: ~S~^, ~_~}~:>~}~}~]"
	  transports))

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
	 :initform nil
	 :documentation
	 "The type of the informer for which the creation failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to create RSB informer in the channel ~
designated by ~S and type ~
~S~/rsb::maybe-print-transport-configuration/~/more-conditions::maybe-print-cause/~@:>"
	     (participation-failed-scope      condition)
	     (informer-creation-failed-type   condition)
	     (participation-failed-transports condition)
	     condition)))
  (:documentation
   "This error is signaled when an attempt to create an informer
fails."))

(define-condition no-transports (participation-failed)
  ()
  (:default-initargs
   :transports nil
   :references (list (documentation-ref/rsb-manual "Troubleshooting")
		     (documentation-ref/rsb-manual "Concepts" "URIs")
		     (documentation-ref/rsb-manual "Common" "Common Environment Variables")
		     (documentation-ref/rsb-bug)))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to participate in the channel ~
designated by ~S because no transports have been selected.~@:>"
	     (scope-string (participation-failed-scope condition)))))
  (:documentation
   "This error is signaled when the creation of a participant fails
because an empty list of transports is supplied."))


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
	     (type-of (event-data (type-error-datum condition)))
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
to or a sub-scope of the expected scope ~A.~:@>"
	     (event-scope (invalid-event-event condition))
	     (invalid-event-event condition)
	     (invalid-event-expected-scope condition))))
  (:documentation
   "This error is signaled when an event is used in a context in which
its scope is not valid."))
