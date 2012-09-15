;;; protocol.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsb.serialization)


;;; Event (de)serialization protocol
;;

(defgeneric notification->event (serialization notifcation)
  (:documentation
   "Convert NOTIFICATION into an `event' instance and return the
event. If message cannot be converted into an event, return nil
instead. Signal a `decoding-error' if something goes wrong."))

(defgeneric event->notification (serialization event)
  (:documentation
   "Convert EVENT into a notification for sending via
CONNECTOR. Return the notification. If EVENT cannot be converted into
a notification, maybe return nil, depending on the error handling
policy. Signal an `encoding-error' if something goes wrong."))


;;; Default behavior
;;

(defmethod notification->event :around ((serialization t)
					(notification  t))
  "TODO(jmoringe): document"
  (with-condition-translation
      (((error decoding-error)
	:encoded          (list notification) ;;; TODO(jmoringe): hack
	:format-control   "~@<After unpacking, the ~
notification~_~A~_could not be converted into an event.~:@>"
	:format-arguments (list (with-output-to-string (stream)
				  (describe notification stream)))))
    (call-next-method)))

(defmethod event->notification :around ((serialization t)
					(event         t))
  (with-condition-translation
      (((error encoding-error)
	:event            event
	:format-control   "~@<The event ~S could not ~
be packed using protocol buffer serialization.~@:>"
	:format-arguments (list event)))
    (call-next-method)))


;;;
;;

(class-families:define-findable-class-family serialization
    "This class families provides implementations of RSB event
serialization mechanisms.")
