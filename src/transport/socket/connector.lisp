;;;; connector.lisp --- Superclass for socket-based connectors.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass connector (rsb.transport:connector
                     conversion-mixin)
  ((bus      :accessor connector-bus
             :documentation
             "Stores the bus object representing the socked-based bus
              to which the connector provides access.")
   ;; Option slots
   (host     :initarg  :host
             :type     string
             :reader   connector-host
             :initform *default-host*
             :documentation
             "The name of the host on which the server is listener in case of clients and the bind address in case of the server.")
   (port     :type     (unsigned-byte 16)
             :reader   connector-port
             :initform *default-port*
             :documentation
             "The port on which the server is listening in case of clients and the port on which connections should be accepted in case of the server.")
   (portfile :initarg  :portfile
             :type     (or null string)
             :reader   connector-portfile
             :initform nil
             :documentation
             "Optionally stores the name of a file (or \"-\" for standard-output) into which an automatically assigned port number should be written when acting as server on an arbitrary free port (indicated by port number 0).")
   (server?  :type     (or boolean (eql :auto))
             :reader   connector-server?
             :initform :auto
             :documentation
             "Controls whether the connector takes the server or client role for the bus.")
   (nodelay? :type     boolean
             :reader   connector-nodelay?
             :initform t
             :documentation
             "Controls whether decreased throughput should be traded for reduced latency by the connector. For TCP connections this means the TCP_NODELAY option should be set on the socket implementing the bus connection."))
  (:metaclass connector-class)
  (:transport :socket)
  (:default-initargs
   :host (missing-required-initarg 'connector :host)
   :port (missing-required-initarg 'connector :port))
  (:options
   (:host       &slot)
   (:port       &slot port)
   (:portfile   &slot)
   (:server     &slot server?)
   (:tcpnodelay &slot nodelay?))
  (:documentation
   "This class serves as a superclass for connector classes that
    employ socked-based bus access."))

;; TODO(jmoringe, 2011-12-14): temp solution until config system works properly
(defmethod initialize-instance :after ((instance connector)
                                       &key
                                       port
                                       portfile
                                       server?
                                       server
                                       tcpnodelay
                                       nodelay?)
  (let ((port     (etypecase port
                    (string
                     (parse-integer port))
                    ((unsigned-byte 16)
                     port)))
        (server?  (let ((value (or server? server)))
                    (etypecase value
                      ((member t nil :auto)
                       value)
                      (string
                       (cond
                         ((string= value "0")    nil)
                         ((string= value "1")    t)
                         ((string= value "auto") :auto))))))
        (nodelay? (let ((value (or tcpnodelay nodelay?)))
                    (etypecase value
                      (boolean value)
                      (string
                       (cond
                         ((string= value "0") nil)
                         ((string= value "1") t)
                         (t                   (error "~@<Invalid value ~S.~@:>"
                                                     value))))))))
    (when (and (not (eq server? t)) portfile)
      (incompatible-initargs 'connector
                             :portfile portfile
                             :server?  server?))
    (setf (slot-value instance 'port)     port
          (slot-value instance 'server?)  server?
          (slot-value instance 'nodelay?) nodelay?)))

(defmethod notify ((connector connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (let+ (((&structure connector- transport host port server? bus) connector)
         (role (case server?
                 ((t)   :server)
                 ((nil) :client)
                 (t     server?))))
    ;; Depending on whether connecting to the socket-based bus as a
    ;; client or server has been requested, request a suitable bus
    ;; access provider.
    (setf bus (transport-ensure-bus transport host port role connector))

    ;; Notify the bus access provider of the added connector.
    ;; ensure-bus-* already attached CONNECTOR.
    ;; (notify connector bus :attached)
    (when (next-method-p)
      (call-next-method))))

(defmethod notify ((connector connector)
                   (scope     scope)
                   (action    (eql :detached)))
  ;; Notify the bus access provider of the removed connector.
  (notify connector (connector-bus connector) :detached)

  (when (next-method-p)
    (call-next-method)))
