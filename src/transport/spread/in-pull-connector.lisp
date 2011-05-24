;;; in-pull-connector.lisp --- An in-direction, pull-based connector for spread.
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

(defmethod find-transport-class ((spec (eql :spread-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (connector
			     broadcast-processor
			     assembly-mixin)
  ((direction :initform :in-pull))
  (:metaclass connector-class)
  (:documentation
   "DOC"))
;; TODO store one/many converter(s) here and dispatch fully deserialized events?

;; TODO allow connector to continue after failed decoding or dispatching
;; for example, there could be an :around method on handle-message with the appropriate restarts
(defmethod emit ((connector in-pull-connector) (block? t))
  (bind (((:accessors-r/o (connection connector-connection)) connector))
    (iter ;; Maybe block until a notification is received. Try to
	  ;; convert into an event and return the event in case of
	  ;; success. In blocking mode, wait for the next
	  ;; notification.
	  (bind (((:values payload sender destination)
		  (receive-message connection :block? block?))
		 (notification (when payload
				 (pb:unpack payload 'rsb.protocol::notification)))
		 (event        (when notification
				 (notification->event notification connector))))

	    ;; TODO temp
	    (when event
	      (log1 :info "~A: event [~S -> ~S] ~A." connector sender destination event))

	    ;; Due to fragmentation of large events into multiple
	    ;; notifications, we may not obtain an `event' instance
	    ;; from the notification.
	    (when (or event (not block?))
	      (return event))))))
