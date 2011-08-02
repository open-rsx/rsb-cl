;;; restart-message-sender-mixin.lisp --- Provide restarts around send-notification and event->notification.
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

(in-package :rsb.transport)

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
  (iter (restart-case
	    (return-from send-notification (call-next-method))
	  (log (&optional condition)
	    :report (lambda (stream)
		      (format stream "~@<Log a message and ignore ~
the failed sending attempt and continue with the next ~
notification.~@:>"))
	    (log1 :warn connector "Failed to send a notification~@[: ~_~A~]" condition)
	    nil)
	  (continue ()
	    :report (lambda (stream)
		      (format stream "~@<Ignore the failed sending ~
attempt and continue with the next notification.~@:>"))
	    nil))))

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
      (log1 :warn connector "Failed to encode an event~@[: ~_~A~]" condition)
      nil)
    (continue ()
      :report (lambda (stream)
		(format stream "~@<Ignore the failed encoding and ~
continue with the next event.~@:>"))
      nil)))