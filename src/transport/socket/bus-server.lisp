;;;; bus-server.lisp --- A class that accepts connections from bus clients.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; `bus-server' class

(defclass bus-server (bus
                      threaded-receiver-mixin)
  ((socket :reader   bus-socket
           :writer   (setf bus-%socket)
           :documentation
           "Stores the listening socket which accepts client
            connections.")
   (state  :type     (member :active :shutdown)
           :accessor bus-state
           :initform :active
           :documentation
           "Stores the state of the bus server. Currently only used
            during shutdown."))
  (:documentation
   "An instance of this class provides access to a bus through a
listening socket to which bus clients connect. Each client connection
causes a `bus-connection' instance to be added to the list of
connections. These objects are removed when the connections are
closed."))

(defmethod shared-initialize :after ((instance   bus-server)
                                     (slot-names t)
                                     &key
                                     host
                                     port)
  ;; Setup the listening socket.
  (setf (bus-%socket instance)
        (usocket:socket-listen host port
                               :element-type '(unsigned-byte 8)))
  (log:info "~@<~A has opened listen socket~@:>" instance)

  (log:info "~@<~A is starting acceptor thread~@:>" instance)
  (start-receiver instance))

(defmethod notify ((bus     bus-server)
                   (subject (eql t))
                   (action  (eql :detached)))
  ;; Close the listen socket to prevent new client connections and
  ;; interrupt the accept loop.
  (log:info "~@<~A is closing listen socket~@:>" bus)
  (unwind-protect
       (let+ (((&structure bus- socket state) bus))
         (setf state :shutdown)
         #+sbcl (sb-bsd-sockets:socket-shutdown
                 (usocket:socket socket) :direction :input)
         (usocket:socket-close socket)) ; TODO ignore errors?

    ;; Wait for the acceptor thread to exit.
    (log:info "~@<~A is stopping acceptor thread~@:>" bus)
    (unwind-protect
         (stop-receiver bus)

      ;; Close existing connections.
      (call-next-method))))

;;; Accepting clients

(defmethod receive-messages ((bus bus-server))
  (let+ (((&structure bus- connections options (server-socket socket) state) bus)
         ((&flet accept ()
            ;; Try to accept a client. In case of an error, check
            ;; whether the bus socket has been removed, indicating
            ;; orderly shutdown..
            (handler-bind ((usocket:socket-error
                            (lambda (condition)
                              (log:debug "~@<~A encountered a socket ~
                                          error while accepting: ~A~@:>"
                                         bus condition)
                              (when (eq state :shutdown)
                                (exit-receiver)))))
              (usocket:socket-accept
               server-socket :element-type '(unsigned-byte 8))))))
    ;; Main processing loop. Wait for activity on the server socket.
    (log:debug "~@<~A is starting to accept connections~:@>" bus)
    (iter (when-let ((client-socket (accept)))
            ;; Since we create and add the new connection with the bus
            ;; lock held, all events published on BUS after the
            ;; handshake of the new connection completes are
            ;; guaranteed to be delivered to the new connection. Also
            ;; note that the server role of the handshake, sending 4
            ;; bytes, usually does not involve blocking.
            (with-locked-bus (bus)
              (push (apply #'make-instance 'bus-connection
                           :socket    client-socket
                           :handshake :send
                           (remove-from-plist options :portfile))
                    connections))
            (log:info "~@<~A accepted bus client ~
                       ~/rsb.transport.socket::print-socket/~@:>"
                      bus client-socket)))))

;;;

(defmethod print-items:print-items append ((object bus-server))
  (let+ (((&structure-r/o bus- socket state) object))
    `((:state  ,state  "~A "
       ((:before :connection-count)))
      (:socket ,socket " ~:/rsb.transport.socket::print-socket/"
       ((:after :connector-count))))))
