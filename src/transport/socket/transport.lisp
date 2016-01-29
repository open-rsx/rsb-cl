;;;; transport.lisp --- Socket transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defvar *default-host* "localhost"
  "Default host used by the socket-based transport.")

(defvar *default-port* 55555
  "Default port used by the socket-based transport.")

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; `socket-transport' transport class

  (defclass socket-transport (transport)
    ((servers :reader   transport-servers
              :initform (make-hash-table :test #'equalp)
              :documentation
              "Map host names and ports to `bus-servers' instances.")
     (clients :reader   transport-clients
              :initform (make-hash-table :test #'equalp)
              :documentation
              "Map host names and ports to `bus-client' instances.")
     (lock    :reader   transport-lock
              :initform (bt:make-recursive-lock "Bus clients+servers lock")
              :documentation
              "Protects accesses to clients and servers slots."))
    (:documentation
     "The (usually singleton) instance of this represents the socket
      transport.

      The instances manages bus clients and servers corresponding to
      different connector configurations."))

;;; Transport registration

  (register-transport
   :socket
   :transport-class 'socket-transport
   :schemas         :socket
   :wire-type       'nibbles:octet-vector
   :documentation
   "TCP-Socket-based transport for small numbers of communicating processes.

    One of the communicating processes acts as the server, opening a
    TCP-listen-socket. Other processes connect to this socket to send
    and receive events. Within each processes, arbitrary numbers of
    participants can share the respective socket connection of the
    process.

    Since currently all events are independently delivered to all
    processes, regardless of whether the individual processes actually
    contain participants, this transport cannot work efficiently with
    participants distributed across many processes.")

  ) ; eval-when

(#+sbcl sb-ext:defglobal #-sbcl defvar **transport**
        (service-provider:find-provider 'transport :socket))

(declaim #+sbcl (sb-ext:always-bound **transport**)
         (type socket-transport **transport**))

;;; Ensuring bus clients and servers
;;;
;;; Depending on the ROLE parameter, find a bus provider for HOST and
;;; PORT and add CONNECTOR to it. If ROLE is :AUTO, first try to
;;; create a server provider and try to fall back to a client provider
;;; if that fails.

;; Act as server unconditionally.

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (host      string)
                                 (port      integer)
                                 (role      (eql :server!))
                                 (connector t))
  (log:trace "~@<~A is trying to obtain bus server ~A:~D for ~A~@:>"
             transport host port connector)
  (let+ ((options   (make-connection-options connector))
         (port-file (getf options :portfile))
         (host      "0.0.0.0")
         (key       (cons host port))
         ((&structure-r/o transport- servers lock) transport))
    (bt:with-recursive-lock-held (lock)
      (or (when-let ((candidate (gethash key servers)))
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
            (setf (gethash key servers) bus))))))

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (host      string)
                                 (port      integer)
                                 (role      (eql :server))
                                 (connector t))
  (restart-case
      (transport-ensure-bus transport host port :server! connector)
    (retry-as-client ()
      :report (lambda (stream)
                (format stream "~@<Try connecting to the bus at ~
                                ~A:~D as client.~@:>"
                        host port))
      (transport-ensure-bus transport host port :client! connector))))

;; Act as client unconditionally.

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (host      string)
                                 (port      integer)
                                 (role      (eql :client!))
                                 (connector t))
  (log:trace "~@<~A is trying to obtain bus client ~S:~D for ~A~@:>"
             transport host port connector)
  (let+ ((options (make-connection-options connector))
         (key     (cons host port))
         ((&structure-r/o transport- clients lock) transport))
    (bt:with-recursive-lock-held (lock)
      (or (when-let ((candidate (gethash key clients)))
            (with-locked-bus (candidate)
              (when (bus-connectors candidate)
                (check-connection-options (bus-options candidate) options)
                (notify connector candidate :attached)
                candidate)))
          (let ((bus (make-instance 'bus-client
                                    :host    host
                                    :port    port
                                    :options options)))
            (notify connector bus :attached)
            (setf (gethash key clients) bus))))))

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (host      string)
                                 (port      integer)
                                 (role      (eql :client))
                                 (connector t))
  (restart-case
      (transport-ensure-bus transport host port :client! connector)
    (retry-as-server ()
      :report (lambda (stream)
                (format stream "~@<Try to create a server for the ~
                                bus at ~A:~D.~@:>"
                        host port))
      (transport-ensure-bus transport host port :server! connector))))

;; Automatically decide whether to act as bus server or client

(defmethod transport-ensure-bus ((transport socket-transport)
                                 (host      string)
                                 (port      integer)
                                 (role      (eql :auto))
                                 (connector t))
  ;; Try to create a server bus provider and fall back to connecting
  ;; via a client bus provider if the server bus provider could not be
  ;; created.
  (bt:with-recursive-lock-held ((transport-lock transport))
    (handler-case
        (transport-ensure-bus transport host port :server! connector)
      ((or usocket:address-in-use-error
           usocket:address-not-available-error
           #+sbcl sb-bsd-sockets:socket-error)
          (server-condition)
        (with-condition-translation
            (((error socket-bus-auto-connection-error
                     :var           client-condition
                     :cause-initarg nil)
              :format-control   "Failed to get socket-based bus as ~
                                 server:~
                                 ~&~<> ~@;~A~:>~
                                 ~&Failed to get socket-based bus as ~
                                 client:~
                                 ~&~<> ~@;~A~:>"
              :format-arguments (list (list server-condition)
                                      (list client-condition))))
          (transport-ensure-bus transport host port :client! connector))))))
