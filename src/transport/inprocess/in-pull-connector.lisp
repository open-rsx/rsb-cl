;;; in-pull-connector.lisp ---
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

(cl:in-package :rsb.transport.inprocess)


;;; Interface for the in-direction, pull-based connector
;;

(defgeneric connector-queue-count (connector)
  (:documentation
   "Return the number of messages currently queued in CONNECTOR."))


;;; `in-pull-connector' class
;;

(defmethod find-transport-class ((spec (eql :inprocess-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (connector
			     broadcast-processor
			     error-handling-dispatcher-mixin
			     error-handling-pull-receiver-mixin)
  ((queue :type     #+sbcl sb-concurrency:mailbox
	            #-sbcl list
	  :reader   connector-queue
	  #-sbcl    #-sbcl
	  :accessor %connector-queue
	  :initform #+sbcl (sb-concurrency:make-mailbox
			    :name "event queue")
	            #-sbcl nil
	  :documentation
	  "Stores events as they arrive via the message bus.")
   #-sbcl
   (lock      :reader   %connector-queue-lock
	      :initform (bt:make-lock "Connector Queue Lock")
	      :documentation
	      "Protects queue slot from concurrent access.")
   #-sbcl
   (condition :reader   %connector-queue-condition
	      :initform (bt:make-condition-variable
			 :name "Connector Queue Condition")
	      :documentation
	      "Notifies threads waiting for events being enqueued."))
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "Instances of this connector class deliver RSB events within a
process."))

(defmethod connector-queue-count ((connector in-pull-connector))
  #+sbcl (sb-concurrency:mailbox-count
	  (connector-queue connector))
  #-sbcl
  (bt:with-lock-held ((%connector-queue-lock connector))
    (length (connector-queue connector))))

(defmethod notify ((connector in-pull-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (log1 :info connector "Attaching to scope ~S" scope)
  (push connector (by-scope scope)))

(defmethod notify ((connector in-pull-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (log1 :info connector "Detaching from scope ~S" scope)
  (removef (by-scope scope) connector :count 1))

(defmethod handle ((connector in-pull-connector)
		   (event     event))
  "Put EVENT into the queue maintained by CONNECTOR."
  (log1 :info connector "Adding event ~S" event)
  #+sbcl (sb-concurrency:send-message (connector-queue connector) event)
  #-sbcl
  (let+ (((&accessors-r/o
	   (lock      %connector-queue-lock)
	   (condition %connector-queue-condition)) connector))
    (bt:with-lock-held (lock)
      (appendf (%connector-queue connector) (list event))
      (bt:condition-notify condition))))

(defmethod receive-message ((connector in-pull-connector)
			    (block?    (eql nil)))
  "Extract and return one event from the queue maintained by
CONNECTOR, if there are any. If there are no queued events, return
nil."
  #+sbcl (sb-concurrency:receive-message-no-hang
	  (connector-queue connector))
  #-sbcl
  (bt:with-lock-held ((%connector-queue-lock connector))
    (when (connector-queue connector)
      (pop (%connector-queue connector)))))

(defmethod receive-message ((connector in-pull-connector)
			    (block?    t))
  "Extract and return one event from the queue maintained by
CONNECTOR, if there are any. If there are no queued events, block."
  #+sbcl (sb-concurrency:receive-message (connector-queue connector))
  #-sbcl
  (let+ (((&accessors-r/o
	   (lock      %connector-queue-lock)
	   (condition %connector-queue-condition)) connector))
    (bt:with-lock-held (lock)
      (iter (until (connector-queue connector))
	    (bt:condition-wait condition lock))
      (pop (%connector-queue connector)))))

(defmethod emit ((connector in-pull-connector) (block? t))
  (when-let ((event (receive-message connector block?)))
    (dispatch connector event)
    t))

(defmethod print-object ((object in-pull-connector) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A ~A (~D)"
	    (connector-direction object)
	    (connector-relative-url object "/")
	    (connector-queue-count object))))
