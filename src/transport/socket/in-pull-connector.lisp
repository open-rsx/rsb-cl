;;; in-pull-connector.lisp --- In-direction, pull-style socket connector.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:in-package :rsb.transport.socket)

(defmethod find-transport-class ((spec (eql :socket-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (error-handling-pull-receiver-mixin
			     in-connector)
  ((queue :type     lparallel.queue:queue
	  :reader   connector-queue
	  :initform (lparallel.queue:make-queue)
	  :documentation
	  "Stores notifications as they arrive via the message bus."))
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "This class implements in-direction, push-style communication over
a socket."))

(defmethod handle ((connector in-pull-connector)
		   (data      notification))
  "Put DATA into the queue of CONNECTOR for later retrieval."
  (lparallel.queue:push-queue data (connector-queue connector)))

(defmethod receive-message ((connector in-pull-connector)
			    (block?    (eql nil)))
  "Extract and return one event from the queue maintained by
CONNECTOR, if there are any. If there are no queued events, return
nil."
  (lparallel.queue:try-pop-queue (connector-queue connector)))

(defmethod receive-message ((connector in-pull-connector)
			    (block?    t))
  "Extract and return one event from the queue maintained by
CONNECTOR, if there are any. If there are no queued events, block."
  (lparallel.queue:pop-queue (connector-queue connector)))

(defmethod emit ((connector in-pull-connector) (block? t))
  ;; Maybe block until a notification is received. Try to convert into
  ;; an event and return the event in case of success. In blocking
  ;; mode, wait for the next notification.
  (iter (let* ((payload (receive-message connector block?))
	       (event   (when payload
			  (message->event
			   connector payload :undetermined))))

	  ;; Due to non-blocking receive mode and error handling
	  ;; policies, we may not obtain an `event' instance from the
	  ;; notification.
	  (when event
	    (dispatch connector event))
	  (when (or event (not block?))
	    (return event)))))

(defmethod print-object ((object in-pull-connector) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A ~A (~D)"
	    (connector-direction object)
	    (connector-relative-url object "/")
	    (lparallel.queue:queue-count
	     (connector-queue object)))))
