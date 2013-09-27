;;;; in-pull-connector.lisp --- An in-direction, pull-based connector for spread.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.spread)

(defmethod find-transport-class ((spec (eql :spread-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (error-handling-pull-receiver-mixin
			     in-connector)
  ()
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "This class implements pull-style event receiving for the Spread
transport."))

(defmethod emit ((connector in-pull-connector) (block? t))
  ;; Maybe block until a notification is received. Try to convert into
  ;; an event and return the event in case of success. In blocking
  ;; mode, wait for the next notification.
  (iter (let* ((notification (receive-message connector block?))
	       (event (when notification
			(message->event
			 connector notification :undetermined))))

	  ;; Due to fragmentation of large events into multiple
	  ;; notifications, non-blocking receive mode and error
	  ;; handling policies, we may not obtain an `event' instance
	  ;; from the notification.
	  (when event
	    (dispatch connector event))
	  (when (or event (not block?))
	    (return event)))))
