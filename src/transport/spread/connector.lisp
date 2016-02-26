;;;; connector.lisp --- Superclass for spread connectors.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defclass connector (rsb.transport:connector
                     conversion-mixin)
  ((connection :initarg  :connection
               :type     connection
               :reader   connector-connection
               :accessor connector-%connection
               :documentation
               "Stores the connection used by this connector."))
  (:metaclass connector-class)
  (:transport :spread)
  (:options
   (:name string
    :description
    "The name of the spread daemon. Has to be either of the form PORT@HOSTNAME or just PORT. Mutually exclusive with HOST and PORT.")
   (:host string
    :description
    "The hostname of the spread daemon. Mutually exclusive with NAME.")
   (:port (integer 0 65534)
    :default network.spread:*default-port*
    :description
    "The port number of the spread daemon. Mutually exclusive with NAME.")
   (:tcpnodelay boolean
    :default t
    :description
    "Should the TCP_NODELAY option be set on the socket used for Spread communication? Note: currently ignored by Lisp implementation."))
  (:documentation
   "This class serves as a superclass for spread in and out
connectors."))

(defmethod initialize-instance :before ((instance connector)
                                        &key
                                        connection
                                        name
                                        port)
  ;; Make sure that at least one of CONNECTION, NAME and PORT is
  ;; supplied.
  (unless (or connection name port)
    (missing-required-initarg
     'connector :either-connection-or-name-or-port)))

(defmethod shared-initialize :after ((instance connector) (slot-names t)
                                     &key
                                     connection
                                     name
                                     host
                                     port)
  (let+ (((&values name host port) (normalize-daemon-endpoint name host port))
         ((&structure-r/o connector- url) instance))
    (when host
      (setf (puri:uri-host url) host))
    (setf (puri:uri-port url) port)

    ;; Unless a connection has been supplied, connect to the spread
    ;; daemon designated by NAME.
    (unless connection
      (setf (connector-%connection instance)
            (make-instance 'connection :name name)))))

(defmethod notify ((connector connector)
                   (scope     (eql t))
                   (action    (eql :detached)))
  (disconnect (connector-connection connector)))
