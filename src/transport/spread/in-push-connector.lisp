;;; in-push-connector.lisp --- An in-direction, push-based connector for spread.
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


;;; Protocol for in-direction, push-based connector
;;

(defgeneric handle-message (connector payload sender destination)
  (:documentation
   "Process the message from SENDER to DESTINATION consisting PAYLOAD
that arrived through connector."))


;;; `in-push-connector' class
;;

(defmethod find-transport-class ((spec (eql :spread-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (in-connector
			     threaded-receiver-mixin)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "This connector class implements push-style event receiving for the
Spread transport."))

(defmethod receive-messages ((connector in-push-connector))
  (iter (while t)
	(let* ((message (receive-message connector t))
	       ;; Try to convert MESSAGE into one or zero events (in
	       ;; the latter case, EVENT is nil).
	       (event   (message->event connector message :undetermined)))
	  ;; Due to fragmentation of large events into multiple
	  ;; notifications and error handling policies, we may not
	  ;; obtain an `event' instance from the notification.
	  (when event
	    (handle connector event)))))
