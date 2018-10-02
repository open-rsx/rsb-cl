;;;; connector.lisp --- Superclass for spread connectors.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defclass connector (rsb.transport:connector
                     conversion-mixin)
  ((configuration :accessor connector-%configuration
                  :documentation
                  "Stores the parameters that should be used when
                   establishing the actual Spread connection.")
   (bus           :initarg  :bus
                  :type     (or null bus)
                  :reader   bus
                  :accessor %bus
                  :initform nil
                  :documentation
                  "Stores the connection used by this connector."))
  (:metaclass connector-class)
  (:transport :spread)
  (:options
   (:name string
    :description
    #.(format nil "The name of the spread daemon. Has to be either of ~
       the form PORT@HOSTNAME or just PORT. Mutually exclusive with ~
       HOST and PORT."))
   (:host string
    :description
    #.(format nil "The hostname of the spread daemon. Mutually ~
       exclusive with NAME."))
   (:port (integer 0 65534)
    :default network.spread.daemon:*default-port*
    :description
    #.(format nil "The port number of the spread daemon. Mutually ~
       exclusive with NAME."))
   (:tcpnodelay boolean
    :default t
    :description
    #.(format nil "Should the TCP_NODELAY option be set on the socket ~
       used for Spread communication? Note: currently ignored by Lisp ~
       implementation."))
   (:age-limit positive-real
    :default 10
    :description
    #.(format nil "The amount of time after which incomplete ~
       assemblies are pruned. Supplying this option only makes sense ~
       in conjunction with an unreliable communication mode since ~
       incomplete assemblies are never pruned in reliable ~
       communication modes.")))
  (:documentation
   "Superclass for Spread in and out connectors."))

(defmethod initialize-instance :before ((instance connector)
                                        &key
                                        bus
                                        name
                                        port)
  ;; Make sure that at least one of BUS, NAME and PORT is
  ;; supplied.
  (unless (or bus name port)
    (missing-required-initarg 'connector :either-bus-or-name-or-port)))

(defmethod shared-initialize :after ((instance connector) (slot-names t)
                                     &key
                                     bus
                                     (name nil name-supplied?)
                                     (host nil host-supplied?)
                                     (port nil port-supplied?)
                                     tcpnodelay
                                     age-limit)
  (when (or name-supplied? host-supplied? port-supplied?)
    (let+ (((&structure-r/o connector- url) instance)
           ((&values name host port)
            (normalize-daemon-endpoint name host port)))
      ;; Normalize URL.
      (when host
        (setf (puri:uri-host url) host))
      (setf (puri:uri-port url) port)

      (setf (connector-%configuration instance)
            (list :bus bus :name name :host host :port port
                  :tcpnodelay tcpnodelay :age-limit age-limit)))))

;;; (Dis)connecting

(defmethod notify ((recipient connector)
                   (subject   (eql t))
                   (action    (eql :attached)))
  (let+ (((&structure-r/o connector- transport (options %configuration))
          recipient)
         ((&plist-r/o (bus :bus)) options))
    ;; Unless a connection has been supplied, connect to the spread
    ;; daemon designated by OPTIONS.
    (setf (%bus recipient) (or bus
                               (ensure-access transport options recipient)))))

(defmethod notify ((recipient connector)
                   (subject   (eql t))
                   (action    (eql :detached)))
  (let+ (((&accessors (bus %bus)) recipient))
    (notify recipient bus :detached)
    (setf bus nil)))

(defmethod notify ((recipient connector) (subject scope) (action t))
  (notify recipient t action))
