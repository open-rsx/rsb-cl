;;; restart-mixins.lisp --- Provide restarts for connectors.
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


;;; Class `restart-notification-sender-mixin'
;;

(defclass restart-notification-sender-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into connector classes that
have to provide the usual restarts when sending notifications in a
`send-notification' method and converting the events to notifications
in a `event->notification' method."))

(defmethod send-notification :around ((connector    restart-notification-sender-mixin)
				      (notification t))
  "Call the next method with log and ignore restarts installed that
will both retry sending NOTIFICATION, but with and without emitting a
log message respectively. "
  (restart-case
      (call-next-method)
    (log (&optional condition)
      :report (lambda (stream)
		(format stream "~@<Log a message and ignore the failed ~
sending attempt and continue with the next notification.~@:>"))
      (log1 :warn connector "Failed to send a notification~@[: ~_~A~]"
	    condition)
      nil)
    (continue (&optional condition)
      :report (lambda (stream)
		(format stream "~@<Ignore the failed sending attempt ~
and continue with the next notification.~@:>"))
      (declare (ignore condition))
      nil)))

(defmethod event->notification :around ((connector    restart-notification-sender-mixin)
					(notification t))
  "Call the next method with log and ignore restarts installed that
with both cause NOTIFICATION to be discarded, but with and without
emitting a log message respectively."
  (restart-case
      (call-next-method)
    (log (&optional condition)
      :report (lambda (stream)
		(format stream "~@<Log a message and ignore the ~
failed encoding and continue with the next event.~@:>"))
      (log1 :warn connector "Failed to encode an event~@[: ~_~A~]"
	    condition)
      nil)
    (continue (&optional condition)
      :report (lambda (stream)
		(format stream "~@<Ignore the failed encoding and ~
continue with the next event.~@:>"))
      (declare (ignore condition))
      nil)))


;;; Class `restart-message-receiver-mixin'
;;

(defclass restart-message-receiver-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into connector classes that
have to provide the usual restarts when receiving messages in a
`receive-message' method and converting the received messages to
events in a `message->event' method."))

(defmethod receive-message :around ((connector restart-message-receiver-mixin)
				    (block?    t))
  "Call the next method with log and ignore restarts installed that
will both retry receiving a message, but with and without emitting a
log message respectively. "
  (iter (restart-case
	    (return-from receive-message (call-next-method))
	  (log (&optional condition)
	    :report (lambda (stream)
		      (format stream "~@<Log a message and ignore ~
the failed receiving attempt and continue with the next ~
notification.~@:>"))
	    (log1 :warn connector "Failed to receive a notification~@[: ~_~A~]" condition)
	    nil)
	  (continue (&optional condition)
	    :report (lambda (stream)
		      (format stream "~@<Ignore the failed receiving ~
attempt and continue with the next notification.~@:>"))
            (declare (ignore condition))
	    nil))))

(defmethod message->event :around ((connector   restart-message-receiver-mixin)
				   (message     t)
				   (wire-schema t))
  "Call the next method with log and ignore restarts installed that
with both cause the call to return nil instead of an `event' instance,
but with and without emitting a log message respectively."
  (restart-case
      (call-next-method)
    (log (&optional condition)
      :report (lambda (stream)
		(format stream "~@<Log a message and ignore the ~
failed decoding and continue with the next event.~@:>"))
      (log1 :warn connector "Failed to decode a notification~@[: ~_~A~]" condition)
      nil)
    (continue (&optional condition)
      :report (lambda (stream)
		(format stream "~@<Ignore the failed decoding and ~
continue with the next event.~@:>"))
      (declare (ignore condition))
      nil)))
