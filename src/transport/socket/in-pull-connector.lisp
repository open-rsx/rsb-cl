;;; in-pull-connector.lisp --- In-direction, pull-style socket connector.
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

(in-package :rsb.transport.socket)

(defmethod find-transport-class ((spec (eql :socket-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (error-handling-pull-receiver-mixin
			     in-connector)
  ((queue :type     #+sbcl sb-concurrency:mailbox
	  #-sbcl list
	  :reader   connector-queue
	  :initform #+sbcl (sb-concurrency:make-mailbox
			    :name "event queue")
	  #-sbcl nil
	  :documentation
	  "Stores notifications as they arrive via the message bus."))
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "This class implements in-direction, push-style communication over
a socket."))

(defmethod connector-queue-count ((connector in-pull-connector))
  #+sbcl (sb-concurrency:mailbox-count
	  (connector-queue connector))
  #-sbcl (length (connector-queue connector)))

(defmethod handle ((connector in-pull-connector)
		   (data      notification))
  "Put DATA into the queue of CONNECTOR for later retrieval."
  #+sbcl (sb-concurrency:send-message (connector-queue connector) data)
  #-sbcl (appendf (connector-queue connector) data))

(defmethod receive-message ((connector in-pull-connector)
			    (block?    t))
  "Retrieve a notification from the queue of CONNECTOR."
  #+sbcl (sb-concurrency:receive-message (connector-queue connector))
  #-sbcl (error "Not implemented"))

(defmethod emit ((connector in-pull-connector) (block? t))
  ;; Maybe block until a notification is received. Try to convert into
  ;; an event and return the event in case of success. In blocking
  ;; mode, wait for the next notification.
  (iter (bind ((payload (receive-message connector block?))
	       (event   (when payload
			  (message->event
			   connector payload :undetermined))))

	  ;; Due to non-blocking receive mode and error handling
	  ;; policies, we may not obtain an `event' instance from the
	  ;; notification.
	  (when event
	    (dispatch connector event))
	  (when (or event (not block?))
	    (return event)))))

(defmethod print-object ((object in-pull-connector) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A ~A (~D)"
	    (connector-direction object)
	    (connector-relative-url object "/")
	    (connector-queue-count object))))
