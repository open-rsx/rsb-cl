;;;; protocol.lisp --- Protocol used in the socket transport module.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; Transport protocol

(defgeneric transport-ensure-bus (transport host port role connector)
  (:documentation
   "Return (creating it if necessary), a `bus-server' or `bus-client'
    instance for the endpoint designated by HOST and PORT and attach
    CONNECTOR to it.

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

;;; Connection shutdown protocol

(defgeneric shutdown-handshake-for (condition)
  (:documentation
   "Return a keyword indicating the appropriate kind of shutdown
handshake to perform for CONDITION."))

(defmethod shutdown-handshake-for ((condition error))
  "Default behavior consists in not performing a shutdown handshake
when encountering an error condition."
  nil)

(defgeneric disconnect (connection
                        &key
                        abort
                        handshake)
  (:documentation
   "Maybe perform shutdown handshake, stop receiver thread and close
socket of connection.

When ABORT is non-nil, skip the shutdown handshake unconditionally.

HANDSHAKE can be either nil, :send or :receive."))
