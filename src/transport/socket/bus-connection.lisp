;;;; bus-connection.lisp --- Connection class used by bus provider.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

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
   (closing?       :type     (member nil t :send :receive)
                   :reader   connection-closing?
                   :accessor %connection-closing?
                   :initform nil
                   :documentation
                   "Indicates indicates whether the connection is
currently closing and, potentially, the role of the connection within
the shutdown handshake.")
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

  ;; TODO(jmoringe, 2013-02-13): this unportable hack is required
  ;; until usocket adds support. Note redundant initarg above.
  (setf (sb-bsd-sockets::sockopt-tcp-nodelay (usocket:socket (connection-socket instance))) nodelay?)

  ;; If requested, perform handshake in the requested role.
  (when handshake
    (handshake instance :setup handshake)))

(defmethod (setf processor-error-policy) ((new-value  t)
                                          (connection bus-connection))
  "Wrap NEW-VALUE in an error policy that exits the receiver thread
after calling NEW-VALUE."
  (call-next-method (%make-error-policy connection new-value) connection))

;;; Protocol

(defun handshake (connection phase role)
  (let ((stream (usocket:socket-stream (connection-socket connection))))
    (log1 :info connection "Performing ~A role of ~A handshake" role phase)
    ;; TODO temp until we implement shutdown protocol
    (if (and (eq phase :shutdown) (eq role :send))
        (sb-bsd-sockets:socket-shutdown
         (usocket::socket (connection-socket connection))
         :direction :output)
        (ecase role
          (:send
           (write-ub32/le 0 stream)
           (finish-output stream))
          (:receive
           (unless (zerop (read-ub32/le stream))
             (error "~@<Protocol error during ~A role of ~A handshake; ~
expected four 0 bytes.~@:>"
                    role phase)))))
    (log1 :info connection "Performed ~A role of ~A handshake" role phase)))

;;; Receiving

(defmethod receive-message ((connection bus-connection)
                            (block?     t))
  (let* ((stream (usocket:socket-stream (connection-socket connection)))
         (length (handler-case ; TODO temp until we implement shutdown protocol
                     (read-ub32/le stream)
                   (end-of-file ()
                     (funcall (processor-error-policy connection)
                              (make-condition 'connection-shutdown-requested
                                              :connection connection))
                     (abort)))))
    (if (zerop length)
        (progn
          (log1 :info connection "Received shutdown request")
          (funcall (processor-error-policy connection)
                   (make-condition 'connection-shutdown-requested
                                   :connection connection))
          (abort))
        (let* ((buffer   (%ensure-receive-buffer connection length))
               (received (read-sequence buffer stream :end length)))
          (unless (= received length)
            (error "~@<Short read (expected: ~D; got ~D)~@:>"
                   length received))
          (values (cons buffer length) :undetermined)))))

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

;;; Shutdown

(defmethod disconnect ((connection bus-connection)
                       &key
                       abort
                       handshake)
  (check-type handshake (member nil :send :receive))

  (let+ (((&accessors (lock     connection-lock)
                      (closing? %connection-closing?)
                      (socket   connection-socket)) connection))
    ;; Ensure that CONNECTION is not already closing or being closed.
    (bt:with-lock-held (lock)
      (when closing?
        ;; If `disconnect' is called with `handshake' :send, it waits
        ;; for being called with `handshake' :receive from another
        ;; thread.p
        (when (and (eq closing? :send) (eq handshake :receive))
          (setf closing? handshake))
        (return-from disconnect nil))
      (setf closing? (or handshake t)))

    ;; If this really is the initial attempt to disconnect CONNECTION,
    ;; perform a shutdown handshake, if requested, stop the receiver
    ;; thread and close the socket. Note that this will be skipped in
    ;; case of an error-induced shutdown (`handshake' is nil in that
    ;; case).
    (when (and (not abort) handshake)

      ;; Perform "send" part of shutdown handshake. This either
      ;; completes the shutdown sequence (if the remote peer initiated
      ;; it) or initiates it.
      (handshake connection :shutdown :send)

      ;; Wait for the shutdown sequence to complete. This can be the
      ;; case immediately if we initiated it.
      (iter (bt:with-lock-held (lock)
              (until (eq closing? :receive)))
            (repeat 5000)
            (sleep .001))
      (unless (eq closing? :receive)
        (warn "~@<Did not receive acknowledgment of shutdown ~
handshake.~@:>")))

    ;; After the shutdown protocol has hopefully been completed, stop
    ;; the receiver and close the socket.
    (log1 :info connection "Stopping receiver thread")
    (unwind-protect
         ;; If this is called from the receiver thread itself, it will
         ;; just abort and unwind at this point.
         (stop-receiver connection)
      (ignore-errors
       (log1 :info connection "Closing socket")
       (usocket:socket-close socket)))
    ;; Return t to indicate that we actually closed the connection.
    t))

;;;

(defmethod print-object ((object bus-connection) stream)
  (let+ (((&accessors-r/o (socket   connection-socket)
                          (closing? connection-closing?)) object))
    (print-unreadable-object (object stream :type t)
      (format stream "~:[open~;closing: ~:*~S~] ~/rsb.transport.socket::print-socket/"
              closing? socket))))

;;; Utility functions

(defun %make-error-policy (connection &optional function)
  "Return a error policy function that calls FUNCTION and closes
CONNECTION when invoked. "
  (named-lambda bus-connection-error-policy (condition)
      ;; Closing CONNECTION can fail (or at least signal an error) for
      ;; various reasons. Make sure the installed error policy is
      ;; still called.
      (log1 :info connection "Maybe closing and executing error policy ~A due to condition: ~A"
            function condition)
      (unwind-protect ; needed because `disconnect' may unwind.
           (handler-case
               ;; `disconnect' returns nil if CONNECTION was already
               ;; being closed. In that case, we do not have to call
               ;; FUNCTION (because somebody else did/does) and can
               ;; abort the receiver thread right away.
               (let ((handshake (shutdown-handshake-for condition)))
                 (when (not (disconnect connection :handshake handshake))
                   (setf function nil)))
             (error (condition)
               (log1 :warn connection "When executing error policy, error closing connection: ~A"
                     condition)))
        ;; If necessary, execute the original error policy, FUNCTION.
        (when function
          (funcall function condition)))))

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
