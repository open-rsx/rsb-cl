;;;; bus-client.lisp --- A bus provider that uses a client socket.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; `bus-client' class

(defclass bus-client (bus)
  ()
  (:documentation
   "Instances of this class provide access to a bus by means of a
    client socket."))

(defmethod initialize-instance :after ((instance bus-client)
                                       &key
                                       make-socket)
  ;; Add a single connection to INSTANCE. The returned connection is
  ;; guaranteed to have completing the handshake and thus receives all
  ;; events published on the bus afterward.
  (setf (bus-connections instance)
        (list (make-instance 'bus-connection
                             :make-socket make-socket
                             :handshake   :receive))))
