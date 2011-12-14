;;; restart-message-receiver-mixin.lisp --- Provide restarts around receive-message and message->event.
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
	  (continue ()
	    :report (lambda (stream)
		      (format stream "~@<Ignore the failed receiving ~
attempt and continue with the next notification.~@:>"))
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
    (continue ()
      :report (lambda (stream)
		(format stream "~@<Ignore the failed decoding and ~
continue with the next event.~@:>"))
      nil)))
