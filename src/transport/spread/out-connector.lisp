;;; out-connector.lisp ---
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

(defmethod find-transport-class ((spec (eql :spread-out)))
  (find-class 'out-connector))

(defclass out-connector (restart-notification-sender-mixin
			 error-handling-sender-mixin
			 connector)
  ((max-fragment-size :initarg  :max-fragment-size
		      :type     positive-fixnum
		      :reader   connector-max-fragment-size
		      :initform 100000
		      :documentation
		      "Stores the maximum fragment size the connector
should use."))
  (:metaclass connector-class)
  (:direction :out)
  (:options
   (:max-fragment-size positive-fixnum
    :default 100000
    :description
    "The maximum payload size that may be send in a single notification. The value of this options has to be chosen such that the combined sizes of payload and envelope data of notifications remain below the maximum message size allowed by spread."))
  (:documentation
   "A connector for sending data over spread."))

(defmethod handle ((connector out-connector) (event event))
  (bind ((group-names   (scope->groups (event-scope event)))
	 (notifications (event->notification connector event))) ;; TODO only if group is populated?
    ;; Due to large events being fragmented into multiple
    ;; notifications, we obtain a list of notifications here.
    (send-notification connector (cons group-names notifications))))

(defmethod event->notification ((connector out-connector)
				(event     event))
  "Delegate conversion to `event->notifications'. The primary purpose
of this method is performing the conversion with restarts installed."
  (event->notifications
   connector event (connector-max-fragment-size connector)))

(defmethod send-notification ((connector                out-connector)
			      (groups-and-notifications cons))
  "Send each notification using `send-message'. The primary purpose of
this method is sending the notifications with restarts installed."
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 ((group-names . notifications) groups-and-notifications))
    (iter (for notification in notifications)
	  (send-message connection group-names (pb:pack* notification)))))
