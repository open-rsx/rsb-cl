;;;; protocol.lisp --- Protocol used in the socket transport module.
;;;;
;;;; Copyright (C) 2013, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; Transport protocol

(defgeneric make-connection-address (transport connector)
  (:documentation
   "Return a suitable address for TRANSPORT and CONNECTOR.

    The address contains information that uniquely identifies the
    socket used by CONNECTOR. For example, a TCP socket is identified
    the combination of a hostname or IP address and a port."))

(defgeneric make-connection-options (transport connector)
  (:documentation
   "Return a plists of options for TRANSPORT and CONNECTOR.

    These options contain information that should be passed to the
    socket used by CONNECTOR but is not necessary to uniquely identify
    it (i.e. not part of the address). For example, the TCP_NODELAY
    option has to be passed to the socket but is not part of its
    address."))

(defgeneric check-connection-options (transport bus-options connector-options)
  (:documentation
   "Signal an error if the option plists BUS-OPTIONS and
    CONNECTOR-OPTIONS contain conflicting properties."))

(defgeneric transport-ensure-bus (transport role connector address
                                  &rest options)
  (:documentation
   "Return, maybe create, a bus for TRANSPORT, ROLE, CONNECTOR and ADDRESS.

    Return (creating it if necessary), a `bus-server' or `bus-client'
    instance for the endpoint designated by ADDRESS as well as OPTIONS
    and attach CONNECTOR to it.

    If such a bus already exists, ensure, in a manner specific to
    TRANSPORT, that OPTIONS are compatible to options of the existing
    bus.

    Attaching CONNECTOR marks the `bus-client' or `bus-client'
    instance as being in use and protects it from being destroyed in a
    race condition situation.

    ROLE can have the following values:

    :auto

      Automatically determine whether to act as server or client.

    :server!

      Act as server.

    :server

      Try to act as server, establishing a restart to try acting as
      client in case of a problem.

    :client!

      Act as client

    :client

      Try to act as client, establishing a restart to try acting as
      server in case of a problem."))

;;; Default behavior

(defmethod check-connection-options :around ((transport          t)
                                             (bus-options        t)
                                             (connectorr-options t))
  (with-simple-restart (continue "~@<Ignore the incompatibility and ~
                                  use the existing bus object.~@:>")
    (call-next-method)))

(defmethod check-connection-options ((transport         t)
                                     (bus-options       t)
                                     (connector-options t)))

;;; Connection shutdown protocol

(defgeneric shutdown-handshake-for (condition)
  (:documentation
   "Return a keyword indicating the appropriate kind of shutdown
    handshake to perform for CONDITION."))

(defmethod shutdown-handshake-for ((condition error))
  ;; Default behavior consists in not performing a shutdown handshake
  ;; when encountering an error condition.
  nil)

(defgeneric disconnect (connection
                        &key
                        abort
                        handshake)
  (:documentation
   "Maybe perform shutdown handshake, stop receiver thread and close
    socket of connection.

    When ABORT is non-nil, skip the shutdown handshake
    unconditionally.

    HANDSHAKE can be either nil, :send or :receive."))

;;; Socket service

(service-provider:define-service socket
  (:documentation
   "Providers of this service create different kinds of sockets.

    The kind of socket to create is specified as a pair of the desired
    address family and an indication of whether an active or passive
    socket should be created."))

(defun make-socket (address-family connect
                    &rest options &key &allow-other-keys)
  (log:debug "~@<Creating ~A ~A socket with options ~{~S~^ ~}~@:>"
             address-family connect options)
  (let ((key (list address-family connect)))
    (declare (dynamic-extent key))
    (apply #'service-provider:make-provider 'socket key options)))
