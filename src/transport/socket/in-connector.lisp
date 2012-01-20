;;; in-connector.lisp ---
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

(defclass in-connector (connector
			restart-message-receiver-mixin
			broadcast-processor
			expose-wire-schema-mixin)
  ((scope :type     scope
	  :accessor connector-scope
	  :documentation
	  "Stores the scope to which the connector is attached."))
  (:metaclass connector-class)
  (:documentation
   "Superclass for in-direction socket connectors. Instances of this
class observe a bus (which owns the actual socket) when attached and
queue received events for delivery."))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (setf (connector-scope connector) scope)
  (call-next-method)
  (push connector (handlers (connector-bus connector))))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (removef (handlers (connector-bus connector)) connector)
  (call-next-method))

(defmethod message->event ((connector    in-connector)
			   (notification notification)
			   (wire-schema  t))
  (bind (((:accessors-r/o
	   (converter           connector-converter)
	   (expose-wire-schema? connector-expose-wire-schema?)) connector))

    ;; If message could be unpacked into a `notification' instance,
    ;; try to convert it, and especially its payload, into an `event'
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
    (handler-bind
	((error #'(lambda (condition)
		    (error 'decoding-error
			   :encoded          (list notification) ;;; TODO(jmoringe): hack
			   :format-control   "~@<After unpacking, the ~
notification~_~A~_could not be converted into an event.~:@>"
			   :format-arguments `(,(with-output-to-string (stream)
								       (describe notification stream)))
			   :cause            condition))))
      (notification->event converter notification
			   :expose-wire-schema? expose-wire-schema?))))
