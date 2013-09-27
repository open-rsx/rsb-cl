;;;; protocol.lisp --- Protocol used in the socket transport module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.socket)


;;; Connection shutdown protocol
;;

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
