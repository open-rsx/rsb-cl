;;;; threaded-message-receiver-mixin.lisp --- Mixin class for threaded notification receiving connector classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport)

(defclass threaded-message-receiver-mixin (threaded-receiver-mixin)
  ()
  (:documentation
   "This mixin class combines receiving of messages and management of
a dedicated thread for receiving messages. It can therefore supply a
default implementation of the receive loop which runs in the receiver
thread."))

(defmethod receive-messages ((connector threaded-message-receiver-mixin))
  "Receive a message that can be decoded into an event. Return the
event."
  (iter (let+ (((&values notification wire-schema)
		(receive-message connector t))
	       ;; Try to convert NOTIFICATION into one or zero events
	       ;; (in the latter case, EVENT is nil).
	       (event (when notification
			(message->event connector notification wire-schema))))
	  ;; Due to fragmentation of large events into multiple
	  ;; notifications and error handling policies, we may not
	  ;; obtain an `event' instance from the notification.
	  (when event
	    (dispatch connector event)))))
