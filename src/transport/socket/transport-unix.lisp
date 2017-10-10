;;;; transport-unix.lisp --- UNIX-domain-socket-based transport.
;;;;
;;;; Copyright (C) 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass unix-socket-transport (socket-transport)
  ((address-family :allocation :class
                   :initform   :local/abstract))
  (:documentation
   "The UNIX domain socket transport."))

(register-transport
 :unix-socket
 :transport-class 'unix-socket-transport
 :schemas         '(:unix :unix-socket)
 :wire-type       'nibbles:octet-vector
 :remote?         nil
 :documentation
 #.(format nil "UNIX domain socket-based transport for same-computer ~
    communication.~@
    ~@
    One of the communicating processes acts as the server, opening a ~
    listening UNIX domain socket. Other processes (running on the same ~
    machine) connect to this socket to send and receive events. Within ~
    each processes, arbitrary numbers of participants can share the ~
    respective socket connection of the process.~@
    ~@
    ~A"
           +backscatter-warning+))
