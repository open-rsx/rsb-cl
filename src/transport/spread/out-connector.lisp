;;; out-connector.lisp ---
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

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
