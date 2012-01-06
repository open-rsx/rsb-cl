;;; threaded-message-receiver-mixin.lisp --- Mixin class for threaded notification receiving connector classes.
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

(cl:in-package :rsb.transport)

(defclass threaded-message-receiver-mixin (threaded-receiver-mixin)
  ()
  (:documentation
   "This mixin class combines receiving of messages and management of
a dedicated thread for receiving messages. It can therefore supply a
default implementation of the receive loop which runs in the receiver
thread."))

(defmethod receive-messages ((connector threaded-message-receiver-mixin))
  "Receive a message that can be decoded into an event. Return the
event."
  (iter (let+ (((&values notification wire-schema)
		(receive-message connector t))
	       ;; Try to convert NOTIFICATION into one or zero events
	       ;; (in the latter case, EVENT is nil).
	       (event (message->event connector notification wire-schema)))
	  ;; Due to fragmentation of large events into multiple
	  ;; notifications and error handling policies, we may not
	  ;; obtain an `event' instance from the notification.
	  (when event
	    (dispatch connector event)))))
