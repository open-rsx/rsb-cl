;;; in-connector.lisp --- Superclass for in-direction connector classes.
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

(in-package :rsb.transport.spread)


;;; In-direction connector protocol
;;

(defgeneric message->event (data connector)
  (:documentation
   "Try to convert DATA into one or zero events. Accordingly,
return either an `event' instance or nil. Signal a `decoding-error' if
something goes wrong."))


;;; `in-connector' class
;;

(defclass in-connector (connector
			broadcast-processor
			assembly-mixin
			conversion-mixin)
  ()
  (:metaclass connector-class)
  (:documentation
   "This class is intended to be used as a superclass of in-direction
connector classes for Spread."))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 (group-name (scope->group scope))) ;; TODO pass a private/thread-local scope cache?
    (ref-group connection group-name))
  :implemented)

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 (group-name (scope->group scope)))
    (unref-group connection group-name))
  :implemented)

(defmethod message->event ((data      simple-array)
			   (connector in-connector))
  (bind (((:accessors-r/o (pool      connector-assembly-pool)
			  (converter connector-converter)) connector)
	 notification)

    ;; Try to unpack DATA into a `notification' instance. Signal
    ;; `decoding-error' if that fails.
    (handler-case
	(setf notification (pb:unpack data 'rsb.protocol::notification))
      (error (condition)
	(error 'decoding-error
	       :encoded          data
	       :format-control   "~@<The data could not be unpacked as a ~
protocol buffer of kind ~S.~:@>"
	       :format-arguments '(rsb.protocol::notification)
	       :cause            condition)))

    ;; If DATA could be unpacked into a `notification' instance, try
    ;; to convert it, and especially its payload, into an `event'
    ;; instance and an event payload. There are three possible
    ;; outcomes:
    ;; 1. The notification (maybe in conjunction with previously
    ;;    received notifications) forms a complete event
    ;;    a) The payload conversion succeeds
    ;;       In this case, an `event' instance is returned
    ;;    b) The payload conversion fails
    ;;       In this case, an error is signaled
    ;; 2. The notification does not form a complete event
    ;;    In this case, nil is returned.
    (handler-case
	(notification->event pool converter notification)
      (error (condition)
	(error 'decoding-error
	       :encoded          data
	       :format-control   "~@<After unpacking, the notification ~
~S could not be converter into an event.~:@>"
	       :format-arguments `(,notification)
	       :cause            condition)))))
