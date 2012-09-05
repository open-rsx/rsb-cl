;;; bus-connection.lisp --- Connection class used by bus provider.
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

(cl:in-package :rsb.transport.socket)

(defclass bus-connection (broadcast-processor
			  threaded-message-receiver-mixin
			  restart-message-receiver-mixin
			  error-handling-push-receiver-mixin
			  restart-notification-sender-mixin
			  error-handling-sender-mixin)
  ((socket         :reader   connection-socket
		   :writer   (setf %connection-socket)
		   :documentation
		   "Stores the socket through which access to he bus
is implemented.")
   (receive-buffer :initarg  :receive-buffer
		   :type     (or null octet-vector)
		   :accessor %connection-receiver-buffer
		   :initform nil
		   :documentation
		   "Static (occasionally enlarged) buffer for
receiving and unpacking serialized notifications.")
   (send-buffer    :initarg  :send-buffer
		   :type     (or null octet-vector)
		   :accessor %connection-send-buffer
		   :initform nil
		   :documentation
		   "Static (occasionally enlarged) buffer for packing
and sending notifications.")
   (closing?       :type     boolean
		   :reader   connection-closing?
		   :accessor %connection-closing?
		   :initform nil
		   :documentation
		   "Stores a Boolean which indicates whether the
connection is currently closing.")
   (lock           :reader   connection-lock
		   :initform (bt:make-lock "Connection lock")
		   :documentation
		   "Stores a lock that protects the connection from
concurrent modifications. Currently, this is only used to prevent
parallel attempts to close the connection."))
  (:documentation
   "Instances of this class manage connections to/from clients of a
socket-based bus. A bus connection is a bi-direction stream of
notifications which are sent and received by participants in different
processes. Client processes use `bus-connection' instances to connect
to socket-based buses and server processes providing these buses
maintain one `bus-connection' instance for each connected client
process.

When a process is connected to a socket-based bus as a client, the
process uses a single `bus-connection' instance for all participants
in the process. Similarly, a process that provides a socket-based bus
as a server creates `bus-connection' instances for remote processes,
but shares these among participants in the process."))

(defmethod shared-initialize :after ((instance   bus-connection)
                                     (slot-names t)
                                     &key
				     error-policy
				     socket
				     host
				     port
				     (nodelay?    t)
				     handshake)
  ;; Install error policy and socket (opening it, if specified via
  ;; HOST and PORT).
  (setf (processor-error-policy instance) error-policy
	(%connection-socket instance)
	(cond
	  (socket)
	  ((and host port)
	   (usocket:socket-connect host port
				   :element-type '(unsigned-byte 8)
				   :nodelay      nodelay?))))

  ;; If requested, perform handshake in the requested role.
  (let ((stream (usocket:socket-stream (connection-socket instance))))
    (case handshake
      (:send
       (write-ub32/le 0 stream)
       (force-output stream))
      (:receive
       (unless (zerop (read-ub32/le stream))
	 (error "~@<Protocol error during handshake; expected four 0 ~
bytes.~@:>"))))))

(defmethod (setf processor-error-policy) ((new-value  t)
					  (connection bus-connection))
  "Wrap NEW-VALUE in an error policy that exits the receiver thread
after calling NEW-VALUE."
  (call-next-method (%make-error-policy connection new-value) connection))


;;; Receiving
;;

(defmethod receive-message ((connection bus-connection)
			    (block?     t))
  (let* ((stream   (usocket:socket-stream (connection-socket connection)))
	 (length   (read-ub32/le stream))
	 (buffer   (%ensure-receive-buffer connection length))
	 (received (read-sequence buffer stream :end length)))
    (unless (= received length)
      (error "~@<Short read (expected: ~D; got ~D)~@:>"
	     length received))
    (values (cons buffer length) :undetermined)))

(defmethod receive-message ((connection bus-connection)
			    (block?     (eql nil)))
  ;; Check whether reading would block and only continue if not.
  (let ((stream (usocket:socket-stream (connection-socket connection))))
    (when (or block? (listen stream))
      (call-next-method))))

(defmethod message->event ((connection  bus-connection)
			   (message     cons)
			   (wire-schema t))
  ;; The whole static buffer and the length of the relevant
  ;; subsequence.
  (declare (type (cons octet-vector (unsigned-byte 32)) message))

  ;; Try to unpack MESSAGE into a `notification' instance. Signal
  ;; `decoding-error' if that fails.
  (let+ (((data . length) message))
    (with-condition-translation
	(((error decoding-error)
	  :encoded          (subseq data 0 length)
	  :format-control   "~@<The wire-data ~S could not be unpacked ~
as a protocol buffer of kind ~S.~:@>"
	  :format-arguments (list (subseq data 0 length) 'notification)))
      (pb:unpack data 'notification 0 length))))


;;; Sending
;;

(defmethod send-notification ((connection   bus-connection)
			      (notification cons))
  (declare (type (cons octet-vector (unsigned-byte 32)) notification))

  (let ((stream (usocket:socket-stream (connection-socket connection))))
    (write-ub32/le (cdr notification) stream)
    (write-sequence (car notification) stream :end (cdr notification))
    (force-output stream)))

(defmethod event->notification ((connection bus-connection)
				(event      notification))
  ;; Pack EVENT into an octet-vector.
  (with-condition-translation
      (((error encoding-error)
	:event            event
	:format-control   "~@<The event ~S could not ~
be packed using protocol buffer serialization.~@:>"
	:format-arguments (list event)))
    (let* ((length (pb:packed-size event))
	   (buffer (%ensure-send-buffer connection length)))
      (declare (type fixnum length))
      (pb:pack event buffer)
      (cons buffer length))))

(defmethod handle ((connection bus-connection)
		   (event      notification))
  (send-notification connection (event->notification connection event)))


;;;
;;

(defmethod close ((connection bus-connection)
		  &key abort)
  (declare (ignore abort))

  (let+ (((&accessors (lock     connection-lock)
		      (closing? %connection-closing?)
		      (socket   connection-socket)) connection))
    ;; Ensure that CONNECTION is not already closing or being closed.
    (bt:with-lock-held (lock)
      (when closing?
	(return-from close nil))
      (setf closing? t))

    ;; If this really is the initial attempt to close CONNECTION, stop
    ;; the receiver thread and close the socket.
    (unwind-protect
	 (progn
	   ;; If there is pending input data, give the receiving
	   ;; machinery 1 second to retrieve it.
	   (iter (while (ignore-errors
			 (listen (usocket:socket-stream socket))))
		 (repeat 100)
		 (sleep .01))

	   ;; After pending input data hopefully has been retrieved,
	   ;; stop the receiver.
	   (log1 :info connection "Stopping receiver thread")
	   (stop-receiver connection))

      ;; Try to flush out pending output data and close the socket.
      (ignore-errors (finish-output (usocket:socket-stream socket)))
      (ignore-errors (usocket:socket-close socket)))
    ;; Return t to indicate that we actually closed the connection.
    t))


;;;
;;

(defmethod print-object ((object bus-connection) stream)
  (let+ (((&accessors-r/o (socket   connection-socket)
			  (closing? connection-closing?)) object))
    (print-unreadable-object (object stream :type t)
      (format stream "~:[open~;closing~] ~/rsb.transport.socket::print-socket/"
	      closing? socket))))


;;; Utility functions
;;

(defun %make-error-policy (connection &optional function)
  "Return a error policy function that calls FUNCTION and closes
CONNECTION when invoked. "
  #'(lambda (condition)
      ;; Closing CONNECTION can fail (or at least signal an error) for
      ;; various reasons. Make sure the installed error policy is
      ;; still called.
      (log1 :info connection "Closing and executing error policy ~A due to condition: ~A"
	    function condition)
      (unwind-protect
	   (handler-case
	       ;; `close' returns nil if CONNECTION was already being
	       ;; closed. In that case, we do not have to call
	       ;; FUNCTION (because somebody else did/does) and can
	       ;; abort the receiver thread right away.
	       (when (not (close connection))
		 (setf function nil))
	     (error (condition)
	       (log1 :warn connection "When executing error policy, error closing connection: ~A"
		     condition)))
	;; If necessary, execute the original error policy, FUNCTION.
	(if function
	    (funcall function condition)
	    (stop-receiver connection)))))

(macrolet
    ((define-ensure-buffer (name accessor)
       `(progn
	  (declaim (inline ,name))

	  (defun ,name (connection size)
	    "Return a suitable buffer for SIZE, creating or enlarging
it first, if necessary."
	    (or (when-let ((buffer (,accessor connection)))
		  (locally (declare (type octet-vector buffer))
		    (when (>= (length buffer) size)
		      buffer)))
		(setf (,accessor connection)
		      (make-octet-vector size)))))))

  (define-ensure-buffer %ensure-receive-buffer %connection-receiver-buffer)
  (define-ensure-buffer %ensure-send-buffer    %connection-send-buffer))
