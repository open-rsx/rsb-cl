;;;; connector.lisp --- Superclass for socket-based connectors.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defvar *default-host* "localhost"
  "Default host used by the socket-based transport.")

(defvar *default-port* 55555
  "Default port used by the socket-based transport.")

(defclass connector (rsb.transport:connector
                     conversion-mixin)
  ((bus      :accessor connector-bus
             :documentation
             "Stores the bus object representing the socked-based
bus to which the connector provides access.")
   ;; Option slots
   (host     :initarg  :host
             :type     string
             :reader   connector-host
             :initform *default-host*
             :documentation
             "The name of the host on which the server is listener in case of clients and the bind address in case of the server.")
   (port     :initarg  :port
             :type     t  ;; TODO (unsigned-byte 16)
             :reader   connector-port
             :initform *default-port*
             :documentation
             "The port on which the server is listening in case of clients and the port on which connections should be accepted in case of the server.")
   (server?  :initarg  :server
             :initarg  :server?
             :type     t ;; TODO (or boolean (eql :auto))
             :reader   connector-server?
             :initform :auto
             :documentation
             "Controls whether the connector takes the server or client role for the bus.")
   (nodelay? :initarg  :tcpnodelay
             :initarg  :nodelay?
             :type     t
             :reader   connector-nodelay?
             :initform t
             :documentation
             "Controls whether decreased throughput should be traded for reduced latency by the connector. For TCP connections this means the TCP_NODELAY option should be set on the socket implementing the bus connection."))
  (:default-initargs
   :host (missing-required-initarg 'in-connector :host)
   :port (missing-required-initarg 'in-connector :port))
  (:metaclass connector-class)
  (:wire-type octet-vector)
  (:schemas   :socket)
  (:options
   (:host       &slot)
   (:port       &slot)
   (:server     &slot)
   (:tcpnodelay &slot))
  (:documentation
   "This class serves as a superclass for connector classes that
employ socked-based bus access."))

;; TODO(jmoringe, 2011-12-14): temp solution until config system works properly
(defmethod shared-initialize :after ((instance   connector)
                                     (slot-names t)
                                     &key
                                     port
                                     server?
                                     server
                                     tcpnodelay
                                     nodelay?)
  (setf (slot-value instance 'port)
        (etypecase port
          (string
           (read-from-string port))
          (non-negative-integer
           port))
        (slot-value instance 'server?)
        (let ((value (or server? server)))
          (etypecase value
            ((member t nil :auto)
             value)
            (string
             (cond
               ((string= value "0")    nil)
               ((string= value "1")    t)
               ((string= value "auto") :auto)))))
        (slot-value instance 'nodelay?)
        (let ((value (or tcpnodelay nodelay?)))
          (etypecase value
            (boolean value)
            (string
             (cond
               ((string= value "0") nil)
               ((string= value "1") t)
               (t                   (error "~@<Invalid value ~S.~@:>"
                                           value))))))))

(defmethod notify ((connector connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (let+ (((&accessors (host    connector-host)
                      (port    connector-port)
                      (server? connector-server?)
                      (bus     connector-bus)) connector))
    ;; Depending on whether connecting to the socket-based bus as a
    ;; client or server has been requested, request a suitable bus
    ;; access provider.
    (setf bus (%get-bus host port server? connector))

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

;;; Utility functions

(defun %get-bus (host port server? connector)
  "Depending on SERVER?, find a bus provider for HOST and PORT and add
CONNECTOR to it. If SERVER? is :AUTO, first try to create a server
provider and try to fall back to a client provider if that fails."
  (ecase server?
    ;; Act as server unconditionally.
    ((t)
     (restart-case
         (ensure-bus-server host port connector)
       (retry-as-client ()
         :report (lambda (stream)
                   (format stream "~@<Retry connecting to the bus at ~
                                   ~A:~D as client.~@:>"
                           host port))
         (%get-bus host port nil connector))))

    ;; Act as client unconditionally.
    ((nil)
     (restart-case
         (ensure-bus-client host port connector)
       (retry-as-server ()
         :report (lambda (stream)
                   (format stream "~@<Try to create a server for the ~
                                   bus at ~A:~D.~@:>"
                           host port))
         (%get-bus host port t connector))))

    ;; Try to create a server bus provider and fall back to connecting
    ;; via a client bus provider if the server bus provider could not
    ;; be created.
    (:auto
     (bt:with-recursive-lock-held (*bus-clients-lock*)
       (bt:with-recursive-lock-held (*bus-servers-lock*)
         (handler-case
             (ensure-bus-server host port connector)
           ((or usocket:address-in-use-error
             usocket:address-not-available-error
             #+sbcl sb-bsd-sockets:socket-error) (server-condition)
             (with-condition-translation
                 (((error socket-bus-auto-connection-error
                          :var           client-condition
                          :cause-initarg nil)
                   :format-control "Failed to get socket-based bus as ~
                                    server:~&~<> ~@;~A~:>~&Failed to ~
                                    get socket-based bus as ~
                                    client:~&~<> ~@;~A~:>"
                   :format-arguments (list (list server-condition)
                                           (list client-condition))))
               (ensure-bus-client host port connector)))))))))
