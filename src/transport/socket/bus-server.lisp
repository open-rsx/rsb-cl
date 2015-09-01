;;;; bus-server.lisp --- A class that accepts connections from bus clients.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; Global map of bus servers

(defvar *bus-servers* (make-hash-table :test #'equalp)
  "Map host names and ports to `bus-servers' instances.")

(defvar *bus-servers-lock* (bt:make-recursive-lock "Bus servers lock")
  "A lock that protects accesses to `*bus-servers*'.")

(defun ensure-bus-server (host port connector)
  "Return (creating it if necessary), a `bus-server' instance for the
endpoint designated by HOST and PORT and attach CONNECTOR to it.
Attaching CONNECTOR marks the `bus-server' instance as being in use
and protects it from being destroyed in a race condition situation."
  (declare (ignore host))
  (let* ((options   (make-connection-options connector))
         (port-file (getf options :portfile))
         (host      "0.0.0.0")
         (key       (cons host port)))
    (log:trace "~@<Trying to obtain bus server ~A:~D for ~A~@:>"
               host port connector)
    (bt:with-recursive-lock-held (*bus-servers-lock*)
      (or (when-let ((candidate (gethash key *bus-servers*)))
            (with-locked-bus (candidate)
              (when (bus-connectors candidate)
                (check-connection-options (bus-options candidate) options)
                (maybe-write-port-file
                 port-file candidate (getf (bus-options candidate) :portfile))
                (notify connector candidate :attached)
                candidate)))
          (let ((bus (make-instance 'bus-server
                                    :host    host
                                    :port    port
                                    :options options)))
            (maybe-write-port-file port-file bus)
            (notify connector bus :attached)
            (setf (gethash key *bus-servers*) bus))))))

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
                 (usocket:socket socket) :direction :io)
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

(defmethod print-object ((object bus-server) stream)
  (print-unreadable-object (object stream :type t)
    (let+ (((&structure-r/o bus- connections connectors socket state) object))
     (format stream "~A (S ~D) (C ~D) ~:/rsb.transport.socket::print-socket/"
             state (length connections) (length connectors) socket))))
