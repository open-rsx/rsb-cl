;;; threaded-receiver-mixin.lisp --- A mixin for threaded receiving connectors.
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


;;; Mixin class `threaded-receiver-mixin'
;;

(defclass threaded-receiver-mixin ()
  ((thread            :type     (or null bt:thread)
		      :accessor connector-thread
		      :reader   connector-started?
		      :initform nil
		      :documentation
		      "Stores the receiver thread of the
connector. Additionally used to indicate the state of the connector,
i.e. if non-nil thread is running and did its setup stuff.")
   (control-mutex     :reader   connector-control-mutex
		      :initform (bt:make-recursive-lock
				 "Receiver Control Mutex")
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

(defmethod start-receiver ((connector threaded-receiver-mixin))
  (let+ (((&accessors
	   (control-mutex     connector-control-mutex)
	   (control-condition connector-control-condition)) connector))
    ;; Launch the thread.
    (log1 :info connector "Starting receiver thread")
    (bt:make-thread (curry #'receive-messages connector)
		    :name (format nil "Message Receiver for ~A"
				  connector))

    ;; Wait until the thread has entered `receive-messages' and
    ;; established the catch environment for the `terminate-thread'
    ;; tag.
    (bt:with-lock-held (control-mutex)
      (iter (until (connector-started? connector))
	    (bt:condition-wait control-condition control-mutex)))))

(defmethod stop-receiver ((connector threaded-receiver-mixin))
  (let+ (((&accessors (thread connector-thread)) connector))
    ;; Interrupt the receiver thread and throw our `terminate-thread'
    ;; tag.
    (log1 :info connector "Interrupting receiver thread")
    (bt:interrupt-thread thread #'exit-receiver)

    ;; The thread should be terminating or already have terminated.
    (log1 :info connector "Joining receiver thread")
    (bt:join-thread thread)
    (setf thread nil)))

(defmethod receive-messages :around ((connector threaded-receiver-mixin))
  "Catch the 'terminate tag that is thrown to indicate interruption
requests."
  (catch 'terminate-thread
    ;; Notify the thread which is waiting in `start-receiver' that we
    ;; can catch the 'terminate-thread tag now.
    (let+ (((&accessors
	     (thread            connector-thread)
	     (control-mutex     connector-control-mutex)
	     (control-condition connector-control-condition)) connector))
      (bt:with-lock-held (control-mutex)
	(setf thread (bt:current-thread))
	(bt:condition-notify control-condition)))
    (log1 :info connector "Entering receive loop")
    (call-next-method))
  (log1 :info connector "Left receive loop"))

(defun exit-receiver ()
  "Cause a receiver thread to exit. Has to be called from the receiver
thread."
  (throw 'terminate-thread nil))
