;;; in-pull-connector.lisp ---
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

(in-package :rsb.transport.inprocess)


;;; Interface for the in-direction, pull-based connector
;;

(defgeneric connector-queue-count (connector)
  (:documentation
   "Return the number of messages currently queued in CONNECTOR."))


;;; `in-pull-connector' class
;;

(defmethod find-transport-class ((spec (eql :inprocess-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (connector)
  ((direction :initform :in-pull)
   (queue     :initarg  :queue
	      :type     #+sbcl sb-concurrency:mailbox
	      #-sbcl list
	      :accessor connector-queue
	      :initform #+sbcl (sb-concurrency:make-mailbox
				:name "event queue")
	      #-sbcl nil
	      :documentation
	      "Stores events as they arrive via the message bus."))
  (:metaclass connector-class)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod connector-queue-count ((connector in-pull-connector))
  #+sbcl (sb-concurrency:mailbox-count
	  (connector-queue connector))
  #-sbcl (length (connector-queue connector)))

(defmethod notify ((connector in-pull-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (log1 :info "~S Attaching to scope ~S" connector scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-pull-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (log1 :info "~S detaching from scope ~S" connector scope)
  (removef (by-scope scope) connector :count 1))

(defmethod handle ((connector in-pull-connector)
		   (event     event))
  "Put EVENT into the queue maintained by CONNECTOR."
  (log1 :info "~S adding event ~S" connector event)
  #+sbcl (sb-concurrency:send-message (connector-queue connector) event)
  #-sbcl (appendf (connector-queue connector) event))

(defmethod emit ((connector in-pull-connector) (block? (eql nil)))
  "Extract and return one event from the queue maintained by
CONNECTOR, if there are any. If there are no queued events, return
nil."
  #+sbcl (sb-concurrency:receive-message-no-hang (connector-queue connector))
  #-sbcl (error "Not implemented"))

(defmethod emit ((connector in-pull-connector) (block? t))
  "Extract and return one event from the queue maintained by
CONNECTOR, if there are any. If there are no queued events, block."
  #+sbcl (sb-concurrency:receive-message (connector-queue connector))
  #-sbcl (error "Not implemented"))

(defmethod print-object ((object in-pull-connector) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~D)" (connector-queue-count object))))
