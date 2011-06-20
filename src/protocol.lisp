;;; protocol.lisp ---
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

(in-package :rsb)


;;; Error handling
;;

;; not invoking any restart says processing should be canceled and the
;; stack should be unwound.

(intern "LOG")
;; log restart: log the error and continue

(defun log-error (condition)
  "Invoke the 'log restart, requesting CONDITION to be logging and
processing to be continued."
  (let ((restart (find-restart 'log)))
    (if restart
	(invoke-restart restart  condition)
	(warn "~@<Restart ~S not found; unwinding.~@:>" 'log))))

(intern "IGNORE")
;; ignore restart: ignore the error silently

(defun ignore-error (condition)
  "Invoke the 'ignore restart, requesting processing to be continued
without further actions."
  (declare (ignore condition))
  (let ((restart (find-restart 'ignore)))
    (if restart
	(invoke-restart restart)
	(warn "~@<Restart ~S not found; unwinding.~@:>" 'ignore))))


;;; Component URL protocol
;;

(defgeneric relative-url (component)
  (:documentation
   "Return a relative URL that allows to locate COMPONENT when
anchored at an absolute location like a transport URL."))


;;; Common participant protocol
;;

(defgeneric detach (participant)
  (:documentation
   "Detach PARTICIPANT from the channel in which it participates and
the transport or transports it uses for this participation."))

(defun detach/ignore-errors (participant)
  "Like `detach' but handle errors that occur during detaching by
continuing in a best effort manner instead of signaling."
  (handler-bind
      (((or error #+sbcl sb-ext:timeout)
	#'(lambda (condition)
	    (warn "~@<Error during detaching: ~A~@:>" condition)
	    (let ((ignore-error (find-restart 'ignore-error)))
	      (when ignore-error
		(invoke-restart ignore-error))))))
    (detach participant)))


;;; Common protocol for receiving participants
;;

(defgeneric receiver-filters (receiver)
  (:documentation
   "Return the list of filters associated to the receiving participant
RECEIVER."))

(defgeneric (setf receiver-filters) (new-value receiver)
  (:documentation
   "Set the list of filters associated to the receiving participant
RECEIVER to NEW-VALUE."))


;;; Listener protocol
;;

(defgeneric make-listener (scope
			   &key
			   transports)
  (:documentation
   "Listen to events on the channel designated by SCOPE.
If successful return a `listener' instance. Otherwise an error of type
`listener-creation-failed' is signaled."))

;; TODO unlisten, also use detach?


;;; Reader protocol
;;

(defgeneric make-reader (scope
			 &key
			 transports)
  (:documentation
   "Receive events on the channel designated by SCOPE.
if successful, return a `reader' instance. Otherwise an error of type
`reader-creation-failed' is signaled. The resulting `reader' instance
can be used to receive data in \"pull\" manner using the `receive'
function."))

(defgeneric receive (reader
		     &key
		     block?)
  (:documentation
   "Receive data from the channel in which RECEIVER is
participating. When data is received, it is returned in form of an
`event' instance. If BLOCK? is non-nil, wait for data to become
available if there is none. If BLOCK/ is nil and no data is available,
nil is returned."))


;;; Informer protocol
;;

(defgeneric make-informer (scope type
			   &key
			   transports)
  (:documentation
   "Start publishing data of type TYPE on the channel designated by
SCOPE.
If successful, return an `informer' instance. Otherwise an error of
type `informer-creation-failed' is signaled."))

(defgeneric send (informer data)
  (:documentation
   "Send DATA to participants of the channel in which INFORMER
participates."))
