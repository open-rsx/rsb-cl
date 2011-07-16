;;; restart-message-receiver-mixin.lisp --- Unit tests for the restart-message-receiver-mixin class.
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

(in-package :rsb.transport.test)

(defvar *receive-message-fail?* t)

(defmethod receive-message ((connector restart-message-receiver-mixin)
			    (block?    t))
  (when *receive-message-fail?*
    (error "~@<No message here.~@:>")))

(defvar *message->event-fail?* t)

(defmethod message->event ((connector   restart-message-receiver-mixin)
			   (message     t)
			   (wire-schema t))
  (when *message->event-fail?*
    (error "~@<Event producing failed.~@:>")))

(deftestsuite restart-message-receiver-mixin-root (transport-root)
  ((simple-connector (make-instance 'restart-message-receiver-mixin)))
  (:documentation
   "Unit tests for the `restart-message-receiver-mixin' class."))

(addtest (restart-message-receiver-mixin-root
          :documentation
	  "Smoke test for the :around method on `receive-message'
provided by `restart-message-receiver-mixin-root'.")
  receive-message-smoke

  (bind (((:flet do-one (restart))
	  (setf *receive-message-fail?* t)
	  (handler-bind
	      ((error #'(lambda (condition)
			  (declare (ignore condition))
			  (setf *receive-message-fail?* nil)
			  (invoke-restart (find-restart restart)))))
	    (receive-message simple-connector t))))
    (do-one 'log)
    (do-one 'continue)))

(addtest (restart-message-receiver-mixin-root
          :documentation
	  "Smoke test for the :around method on `message->event'
provided by `restart-message-receiver-mixin-root'.")
  message->event-smoke

  (bind (((:flet do-one (restart))
	  (setf *message->event-fail?* t)
	  (handler-bind
	      ((error #'(lambda (condition)
			  (declare (ignore condition))
			  (setf *message->event-fail?* nil)
			  (invoke-restart (find-restart restart)))))
	    (message->event
	     simple-connector (make-event "/" "bla") :string))))
    (do-one 'log)
    (do-one 'continue)))
