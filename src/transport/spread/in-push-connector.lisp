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

(defgeneric receive-messages (connector)
  (:documentation
   "Receive messages and process them using `handle-message'."))

(defgeneric handle-message (connector payload sender destination)
  (:documentation
   "Process the message from SENDER to DESTINATION consisting PAYLOAD
that arrived through connector."))


;;; `in-push-connector' class
;;

(defmethod find-transport-class ((spec (eql :spread-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (connector
			     broadcast-processor
			     assembly-mixin
			     conversion-mixin)
  ((terminate? :initarg  :terminate?
	       :type     boolean
	       :initform nil
	       :documentation
	       ""))
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "DOC"))

(defmethod initialize-instance :after ((instance in-push-connector)
                                       &key)
  (bt:make-thread (curry #'receive-messages instance)
		  :name "spread receiver thread"))

(defmethod receive-messages ((connector in-push-connector))
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 ((:slots terminate?) connector))
    (iter (until terminate?)
	  (multiple-value-call #'handle-message
	    (values connector) (receive-message connection)))))

;; TODO allow connector to continue after failed decoding or dispatching
;; for example, there could be an :around method on handle-message with the appropriate restarts
(defmethod handle-message ((connector   in-push-connector)
			   (payload     simple-array)
			   (sender      string)
			   (destination list))
  "Try to converter NOTIFICATION into one or zero events. Accordingly,
return either an `event' instance or nil."
  (let* ((notification (pb:unpack payload 'rsb.protocol::notification))
	 (event        (notification->event
			(connector-assembly-pool connector)
			(connector-converter connector)
			notification)))
    ;; Due to fragmentation of large events into multiple
    ;; notifications, we may not obtain an `event' instance from the
    ;; notification.
    (when event
      (handle connector event))))
