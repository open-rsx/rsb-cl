;;;; transport-tcp.lisp --- TCP/IP-based socket transport.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defvar *default-host* "localhost"
  "Default host used by the socket-based transport.")

(defvar *default-port* 55555
  "Default port used by the socket-based transport.")

;;; Transport

(defclass tcp-socket-transport (socket-transport)
  ((address-family :allocation :class
                   :initform   :tcp))
  (:documentation
   "The TCP/IP socket transport."))

(register-transport
 :tcp-socket
 :transport-class 'tcp-socket-transport
 :schemas         '(:socket :tcp-socket)
 :wire-type       'nibbles:octet-vector
 :remote?         t
 :documentation
 #.(format nil "TCP-Socket-based transport for small numbers of ~
    processes.~@
    ~@
    One of the communicating processes acts as the server, opening a ~
    listening TCP socket. Other processes connect to this socket to ~
    send and receive events. Within each processes, arbitrary ~
    numbers of participants can share the respective socket ~
    connection of the process.~@
    ~@
    ~A"
           +backscatter-warning+))

(defmethod service-provider:make-provider
    ((service t) (provider tcp-socket-transport) &rest args &key)
  ;; Normalize supported schemas (:socket and :tcp-socket) to
  ;; :tcp-socket.
  (apply #'call-next-method service provider
         (list* :schema :tcp-socket (remove-from-plist args :schema))))

(defmethod check-connection-options ((tansport          tcp-socket-transport)
                                     (bus-options       list)
                                     (connector-options list))
  ;; All options except portfile have to match when a connector is
  ;; added to an existing bus.
  (iter (for (key value) on connector-options :by #'cddr)
        (when (eq key :portfile)
          (next-iteration))
        (let ((bus-value (getf bus-options key)))
          (unless (equalp bus-value value)
            (error "~@<Incompatible values for option ~S: current ~
                    bus uses value ~S; requested value is ~S.~@:>"
                   key bus-value value)))))

(defmethod transport-ensure-bus ((transport tcp-socket-transport)
                                 (role      (eql :server!))
                                 (connector t)
                                 (address   list)
                                 &rest options)
  ;; Listen on all interfaces.
  (apply #'call-next-method transport role connector
         (list* :host "0.0.0.0" (remove-from-plist address :host))
         options))
