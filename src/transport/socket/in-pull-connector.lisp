;;; in-pull-connector.lisp --- In-direction, pull-style socket connector.
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

(in-package :rsb.transport.socket)

(defmethod find-transport-class ((spec (eql :socket-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (error-handling-pull-receiver-mixin
			     in-connector)
  ((queue :type     #+sbcl sb-concurrency:mailbox
	  #-sbcl list
	  :reader   connector-queue
	  :initform #+sbcl (sb-concurrency:make-mailbox
			    :name "event queue")
	  #-sbcl nil
	  :documentation
	  "Stores notifications as they arrive via the message bus."))
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "This class implements in-direction, push-style communication over
a socket."))

(defmethod connector-queue-count ((connector in-pull-connector))
  #+sbcl (sb-concurrency:mailbox-count
	  (connector-queue connector))
  #-sbcl (length (connector-queue connector)))

(defmethod handle ((connector in-pull-connector)
		   (data      notification))
  "Put DATA into the queue of CONNECTOR for later retrieval."
  #+sbcl (sb-concurrency:send-message (connector-queue connector) data)
  #-sbcl (appendf (connector-queue connector) data))

(defmethod receive-message ((connector in-pull-connector)
			    (block?    t))
  "Retrieve a notification from the queue of CONNECTOR."
  #+sbcl (sb-concurrency:receive-message (connector-queue connector))
  #-sbcl (error "Not implemented"))

(defmethod emit ((connector in-pull-connector) (block? t))
  ;; Maybe block until a notification is received. Try to convert into
  ;; an event and return the event in case of success. In blocking
  ;; mode, wait for the next notification.
  (iter (bind ((payload (receive-message connector block?))
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
	    (connector-queue-count object))))
