;;; threaded-message-receiver-mixin.lisp --- Mixin class for threaded notification receiving connector classes.
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

(in-package :rsb.transport)

(defclass threaded-message-receiver-mixin (message-receiver-mixin
					   threaded-receiver-mixin)
  ()
  (:documentation
   "This mixin class combines receiving of messages and management of
a dedicated thread for receiving messages. It can therefore supply a
default implementation of the receive loop which runs in the receiver
thread."))

(defmethod receive-messages ((connector threaded-message-receiver-mixin))
  "Receive a message that can be decoded into an event. Return the
event."
  (iter (while t)
	(bind (((:values notification wire-schema)
		(receive-message connector t))
	       ;; Try to convert NOTIFICATION into one or zero events
	       ;; (in the latter case, EVENT is nil).
	       (event (message->event connector notification wire-schema)))
	  ;; Due to fragmentation of large events into multiple
	  ;; notifications and error handling policies, we may not
	  ;; obtain an `event' instance from the notification.
	  (when event
	    (handle connector event)))))
