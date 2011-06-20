;;; threaded-receiver-mixin.lisp --- A mixin for threaded receiving connectors.
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


;;; Threaded receiver protocol
;;

(defgeneric start-receiver (connector)
  (:documentation
   "Ask CONNECTOR to start a receiver thread that runs
`receive-messages' until interrupted"))

(defgeneric stop-receiver (connector)
  (:documentation
   "Ask CONNECTOR to stop receiving messages."))

(defgeneric receive-messages (connector)
  (:documentation
   "CONNECTOR receives and processes messages until interrupted."))


;;; Mixin class `threaded-receiver-mixin'
;;

(defclass threaded-receiver-mixin ()
  ((thread            :type     (or null bt:thread)
		      :accessor connector-thread
		      :initform nil
		      :documentation
		      "Stores the receiver thread of the connector.")
   (started?          :accessor connector-started?
		      :initform nil
		      :documentation
		      "Non-nil, if the connector has been
started (i.e. thread is running and did its setup stuff), nil
otherwise.")
   (control-mutex     :reader   connector-control-mutex
		      :initform (bt:make-recursive-lock
				 "Receive Control Mutex")

		      :documentation
		      "Required for thread startup synchronization.")
   (control-condition :reader   connector-control-condition
		      :initform (bt:make-condition-variable
				 :name "Receiver Control Condition")
		      :documentation
		      "Required for thread startup synchronization."))
  (:documentation
   "This mixin class is intended to be mixed into message receiving
connector classes which want do so in a dedicated thread. This mixin
class takes care of managing the starting and joining of the
thread."))

(defmethod notify :after ((connector threaded-receiver-mixin)
			  (scope     scope)
			  (action    (eql :attached)))
  "After attaching to SCOPE, start a receiver thread."
  (start-receiver connector))

(defmethod notify :before ((connector threaded-receiver-mixin)
			   (scope     scope)
			   (action    (eql :detached)))
  "Before detaching from SCOPE, join the receiver thread."
  (stop-receiver connector))

(defmethod start-receiver ((connector threaded-receiver-mixin))
  (log1 :info "~A Starting receiver thread" connector)
  (bind (((:accessors
	   (thread            connector-thread)
	   (control-mutex     connector-control-mutex)
	   (control-condition connector-control-condition)) connector))
    (setf thread (bt:make-thread (curry #'receive-messages connector)
				 :name (format nil "Message Receiver Thread for ~A"
					       connector)))
    ;; Wait until the thread has entered `receive-messages' and
    ;; established the catch environment for the `terminate-thread'
    ;; tag.
    (bt:with-lock-held (control-mutex)
      (iter (until (connector-started? connector))
	    (bt:condition-wait control-condition control-mutex)))))

(defmethod stop-receiver ((connector threaded-receiver-mixin))
  (bind (((:accessors (thread connector-thread)) connector))
    ;; Interrupt the receiver thread and throw our `terminate-thread'
    ;; tag.
    (log1 :info "~A Interrupting receiver thread" connector)
    (bt:interrupt-thread thread #'(lambda () (throw 'terminate-thread nil)))

    ;; The thread should be terminating or already have terminated.
    (log1 :info "~A Joining receiver thread" connector)
    (bt:join-thread thread)
    (setf thread nil)))

(defmethod receive-messages :around ((connector threaded-receiver-mixin))
  "Catch the 'terminate tag that is thrown to indicate interruption
requests."
  (catch 'terminate-thread
    ;; Notify the thread which is waiting in `start-receiver' that we
    ;; can catch the 'terminate-thread tag now.
    (bind (((:accessors
	     (started?          connector-started?)
	     (control-mutex     connector-control-mutex)
	     (control-condition connector-control-condition)) connector))
      (bt:with-lock-held (control-mutex)
	(setf started? t)
	(bt:condition-notify control-condition)))
    (log1 :info "~A Entering receive loop" connector)
    (call-next-method))
  (log1 :info "~A Left receive loop" connector))
