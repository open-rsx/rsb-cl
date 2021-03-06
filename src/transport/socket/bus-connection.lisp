;;;; bus-connection.lisp --- Connection class used by bus provider.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; Utility functions

(defun %make-error-policy (connection &optional function)
  ;; Return a error policy function that calls FUNCTION and closes
  ;; CONNECTION when invoked.
  (named-lambda bus-connection-error-policy (condition)
    ;; Closing CONNECTION can fail (or at least signal an error) for
    ;; various reasons. Make sure the installed error policy is
    ;; still called.
    (unwind-protect ; needed because `disconnect' may unwind.
         (handler-case
             ;; `disconnect' returns nil if CONNECTION was already
             ;; being closed. In that case, we do not have to call
             ;; FUNCTION (because somebody else did/does) and can
             ;; abort the receiver thread right away.
             (let ((handshake (shutdown-handshake-for condition)))
               (log:info "~@<~A is maybe disconnecting ~
                          ~:[without~;~:*with ~A role of~] shutdown ~
                          handshake and executing error policy ~A ~
                          due to condition: ~A~@:>"
                         connection handshake function condition)
               (when (not (disconnect connection :handshake handshake))
                 (setf function nil)))
           (error (condition)
             (log:warn "~@<When ~A executed error policy, error ~
                        closing connection: ~A~@:>"
                       connection condition)))
      ;; If necessary, execute the original error policy, FUNCTION.
      (when function
        (funcall function condition)))))

;; Return a suitable send/receive buffer for a given size, creating or
;; enlarging it first, if necessary.
(macrolet
    ((define-ensure-buffer (name accessor)
       `(progn
          (declaim (inline ,name))
          (defun ,name (connection size)
            (or (when-let ((buffer (,accessor connection)))
                  (locally (declare (type octet-vector buffer))
                    (when (>= (length buffer) size)
                      buffer)))
                (setf (,accessor connection)
                      (make-octet-vector size)))))))

  (define-ensure-buffer %ensure-receive-buffer connection-%receiver-buffer)
  (define-ensure-buffer %ensure-send-buffer    connection-%send-buffer))

;;; `bus-connection'

(defclass bus-connection (broadcast-processor
                          threaded-message-receiver-mixin
                          restart-notification-receiver-mixin
                          error-handling-push-receiver-mixin
                          restart-notification-sender-mixin
                          error-handling-sender-mixin
                          print-items:print-items-mixin)
  ((socket         :reader   connection-socket
                   :writer   (setf connection-%socket)
                   :documentation
                   "Stores the socket through which access to he bus
                    is implemented.")
   (receive-buffer :type     (or null octet-vector)
                   :accessor connection-%receiver-buffer
                   :initform nil
                   :documentation
                   "Static (occasionally enlarged) buffer for
                    receiving and unpacking serialized
                    notifications.")
   (send-buffer    :type     (or null octet-vector)
                   :accessor connection-%send-buffer
                   :initform nil
                   :documentation
                   "Static (occasionally enlarged) buffer for packing
                    and sending notifications.")
   (closing?       :type     (member nil t :send :receive)
                   :reader   connection-closing?
                   :accessor connection-%closing?
                   :initform nil
                   :documentation
                   "Indicates indicates whether the connection is
                    currently closing and, potentially, the role of
                    the connection within the shutdown handshake.")
   (lock           :reader   connection-lock
                   :initform (bt:make-lock "Connection lock")
                   :documentation
                   "Stores a lock that protects the connection from
                    concurrent modifications. Currently, this is only
                    used to prevent parallel attempts to close the
                    connection."))
  (:documentation
   "Instances of this class manage connections to/from clients of a
    socket-based bus.

    A bus connection is a bi-direction stream of notifications which
    are sent and received by participants in different
    processes. Client processes use `bus-connection' instances to
    connect to socket-based buses and server processes providing these
    buses maintain one `bus-connection' instance for each connected
    client process.

    When a process is connected to a socket-based bus as a client, the
    process uses a single `bus-connection' instance for all
    participants in the process. Similarly, a process that provides a
    socket-based bus as a server creates `bus-connection' instances
    for remote processes, but shares these among participants in the
    process."))

(defmethod initialize-instance :after ((instance bus-connection)
                                       &key
                                       socket
                                       make-socket
                                       handshake)
  ;; Install socket (opening it, if specified necessary).
  (setf (connection-%socket instance)
        (cond
          (socket)
          (make-socket
           (funcall make-socket))))

  ;; If requested, perform handshake in the requested role.
  (when handshake
    (handshake instance :setup handshake)))

(defmethod shared-initialize :after ((instance   bus-connection)
                                     (slot-names t)
                                     &key
                                     (error-policy nil error-policy-supplied?))
  ;; Install or change (when reinitializing) error policy.
  (when error-policy-supplied?
    (setf (processor-error-policy instance) error-policy)))

(defmethod (setf processor-error-policy) ((new-value  t)
                                          (connection bus-connection))
  ;; Wrap NEW-VALUE in an error policy that exits the receiver thread
  ;; after calling NEW-VALUE.
  (call-next-method (%make-error-policy connection new-value) connection))

;;; Protocol

(defun handshake (connection phase role)
  (let ((stream (usocket:socket-stream (connection-socket connection))))
    (log:info "~@<~A is performing ~A role of ~A handshake~@:>"
              connection role phase)
    ;; TODO temp until we implement shutdown protocol
    (if (and (eq phase :shutdown) (eq role :send))
        (progn
          (finish-output stream)
          (usocket:socket-shutdown (connection-socket connection) :output))
        (ecase role
          (:send
           (write-ub32/le 0 stream)
           (finish-output stream))
          (:receive
           (unless (zerop (read-ub32/le stream))
             (error "~@<Protocol error during ~A role of ~A handshake; ~
                     expected four 0 bytes.~@:>"
                    role phase)))))
    (log:info "~@<~A performed ~A role of ~A handshake~@:>"
              connection role phase)))

;;; Receiving

(defmethod receive-notification ((connection bus-connection)
                                 (block?     t))
  (let* ((stream (usocket:socket-stream (connection-socket connection)))
         (length (handler-case ; TODO temp until we implement shutdown protocol
                     (read-ub32/le stream)
                   (end-of-file ()
                     (log:info "~@<~A received end-of-file; treating ~
                                as shutdown request~@:>"
                               connection)
                     (funcall (processor-error-policy connection)
                              (make-condition 'connection-shutdown-requested
                                              :connection connection))
                     (abort)))))
    (if (zerop length)
        (progn
          (log:info "~@<~A received shutdown request~@:>" connection)
          (funcall (processor-error-policy connection)
                   (make-condition 'connection-shutdown-requested
                                   :connection connection))
          (abort))
        (let* ((buffer   (%ensure-receive-buffer connection length))
               (received (read-sequence buffer stream :end length)))
          (unless (= received length)
            (error "~@<Short read (expected: ~D; got ~D)~@:>"
                   length received))
          (values (make-wire-notification buffer length) :undetermined))))) ; TODO ownership of BUFFER

(defmethod receive-notification ((connection bus-connection)
                                 (block?     (eql nil)))
  ;; Check whether reading would block and only continue if not.
  (let ((stream (usocket:socket-stream (connection-socket connection))))
    (when (or block? (listen stream))
      (call-next-method))))

(defmethod notification->event ((connection   bus-connection)
                                (notification wire-notification)
                                (wire-schema  t))
  ;; Try to unpack NOTIFICATION into a `notification' instance. Signal
  ;; `decoding-error' if that fails.
  (let+ (((&structure-r/o wire-notification- buffer end) notification))
    (with-condition-translation
        (((error decoding-error)
          :encoded          (subseq buffer 0 end)
          :format-control   "~@<The wire-data could not be unpacked as a ~
                             protocol buffer of kind ~S.~:@>"
          :format-arguments (list 'notification)))
      (pb:unpack buffer 'notification 0 end))))

;;; Sending

(defmethod send-notification ((connection   bus-connection)
                              (notification wire-notification))
  (when (connection-%closing? connection)
    (log:info "~@<~A is dropping a message since it is closing~@:>" connection)
    (return-from send-notification))
  (let+ ((stream (usocket:socket-stream (connection-socket connection)))
         ((&structure-r/o wire-notification- buffer end) notification))
    (write-ub32/le end stream)
    (write-sequence buffer stream :end end)
    (force-output stream)))

(defmethod event->notification ((connection bus-connection)
                                (event      notification))
  ;; Pack EVENT into an octet-vector.
  (with-condition-translation
      (((error encoding-error)
        :event            event
        :format-control   "~@<The event ~S could not be packed using ~
                           protocol buffer serialization.~@:>"
        :format-arguments (list event)))
    (let* ((length (pb:packed-size event))
           (buffer (%ensure-send-buffer connection length)))
      (declare (type notification-index length))
      (pb:pack event buffer)
      (make-wire-notification buffer length))))

(defmethod handle ((sink bus-connection) (data notification))
  (bt:with-lock-held ((connection-lock sink))
    (send-notification sink (event->notification sink data))))

;;; Shutdown

(defmethod disconnect ((connection bus-connection)
                       &key
                       abort
                       handshake)
  (check-type handshake (member nil :send :receive))

  (let+ (((&structure connection- lock (closing? %closing?) socket) connection))
    ;; Ensure that CONNECTION is not already closing or being closed.
    (bt:with-lock-held (lock)
      (when closing?
        ;; If `disconnect' is called with `handshake' :send, it waits
        ;; for being called with `handshake' :receive from another
        ;; thread.
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

    ;; After the shutdown protocol has hopefully been completed, close
    ;; the socket and wait for the receiver thread to exit.
    (unwind-protect
         (ignore-errors
           (log:info "~@<~A is closing socket~@:>" connection)
           (usocket:socket-close socket))

      ;; If this is called from the receiver thread itself, it will
      ;; just abort and unwind at this point.
      (log:info "~@<~A is stopping receiver thread~@:>" connection)
      (stop-receiver connection))
    ;; Return t to indicate that we actually closed the connection.
    t))

;;;

(defmethod print-items:print-items append ((object bus-connection))
  (let+ (((&structure-r/o connection- socket closing?) object))
    `((:closing? ,closing? "~:[open~;closing: ~:*~S~]")
      (:socket   ,socket   " ~/rsb.transport.socket::print-socket/"
       ((:after :closing?))))))
