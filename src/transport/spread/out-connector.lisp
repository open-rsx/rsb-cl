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

(defclass out-connector (connector) ;; is an "event-sink"; there is no class or interface for this
  ((direction :initform :out))
  (:metaclass connector-class)
  (:options
   (:max-fragment-size positive-fixnum
    "The maximum payload size that may be send in a single
notification. The value of this options has to be chosen such that the
combined sizes of payload and envelope data of notifications remain
below the maximum message size allowed by spread."))
  (:documentation
   "A connector for sending data over spread."))

(defmethod rsb.ep:handle ((connector out-connector) (event event))
  (bind ((group-names   (scope->groups (event-scope event)))
	 (notifications (event->notifications event))) ;; TODO only if group is populated?
    ;; Due to large events being fragmented into multiple notifications,
    ;; we obtain a list of notifications here.
    (iter (for notification in notifications)
	  (send-message (slot-value connector 'connection)
			group-names
			(pb::pack1 notification)))))
