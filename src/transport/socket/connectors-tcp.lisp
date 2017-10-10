;;;; connectors-tcp.lisp --- TCP/IP-based socket transport.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; `tcp-connector'

(defclass tcp-connector (connector)
  (;; Option slots
   (host     :type     string
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
   (nodelay? :type     boolean
             :reader   connector-nodelay?
             :initform t
             :documentation
             "Controls whether decreased throughput should be traded for reduced latency by the connector. For TCP connections this means the TCP_NODELAY option should be set on the socket implementing the bus connection."))
  (:default-initargs
   :host (missing-required-initarg 'tcp-connector :host)
   :port (missing-required-initarg 'tcp-connector :port))
  (:metaclass connector-class)
  (:transport :tcp-socket)
  (:options
   (:host       &slot host)
   (:port       &slot port)
   (:portfile   &slot)
   (:tcpnodelay &slot nodelay?))
  (:documentation
   "Superclass for TCP socket connector classes.

    Takes of care of obtaining the `:tcp-socket' transport object as
    well as setting up the TCP socket-specific options host, port,
    portfile and nodelay?."))

;; TODO(jmoringe, 2011-12-14): temp solution until config system works properly
(defmethod initialize-instance :after ((instance tcp-connector)
                                       &key
                                       port
                                       portfile
                                       tcpnodelay
                                       nodelay?)
  (let ((port     (etypecase port
                    (string
                     (parse-integer port))
                    ((unsigned-byte 16)
                     port)))
        (nodelay? (let ((value (or tcpnodelay nodelay?)))
                    (etypecase value
                      (boolean value)
                      (string
                       (cond
                         ((string= value "0") nil)
                         ((string= value "1") t)
                         (t                   (error "~@<Invalid value ~S.~@:>"
                                                     value)))))))
        (server?  (connector-server? instance)))
    (when (and (not (eq server? t)) portfile)
      (incompatible-initargs 'tcp-connector
                             :portfile portfile
                             :server?  server?))
    (setf (slot-value instance 'port)     port
          (slot-value instance 'nodelay?) nodelay?)))

(defmethod make-connection-address ((transport tcp-socket-transport)
                                    (connector tcp-connector))
  (let+ (((&structure-r/o connector- host port) connector))
    (list :host host :port port)))

(defmethod make-connection-options ((transport tcp-socket-transport)
                                    (connector tcp-connector))
  (list :nodelay? (connector-nodelay? connector)))

(defmethod notify :before ((connector tcp-connector)
                           (subject   bus)
                           (action    (eql :attached)))
  (maybe-write-port-file (connector-portfile connector) subject))

;;; Direction-specific connector classes

(defclass tcp-in-pull-connector (tcp-connector
                                 in-pull-connector)
  ()
  (:metaclass connector-class))

(register-connector :tcp-socket :in-pull 'tcp-in-pull-connector)

(defclass tcp-in-push-connector (tcp-connector
                                 in-push-connector)
  ()
  (:metaclass connector-class))


(register-connector :tcp-socket :in-push 'tcp-in-push-connector)

(defclass tcp-out-connector (tcp-connector
                             out-connector)
  ()
  (:metaclass connector-class))

(register-connector :tcp-socket :out 'tcp-out-connector)
