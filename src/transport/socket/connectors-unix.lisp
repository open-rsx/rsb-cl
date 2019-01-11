;;;; connectors-unix.lisp --- UNIX-domain-socket-based transport.
;;;;
;;;; Copyright (C) 2014, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

;;; `unix-connector'

(defclass unix-connector (connector)
  (;; Option slots
   (name :type    string
         :reader  connector-name
         :writer  (setf connector-%name)
         :documentation
         #.(format nil "The name of the UNIX socket in the abstract ~
            namespace.~@
            ~@
            The name is not translated into a filesystem path and ~
            there is no filesystem object object corresponding to the ~
            socket.")))
  (:default-initargs
   :name (missing-required-initarg 'unix-connector :name))
  (:metaclass connector-class)
  (:transport :unix-socket)
  (:options
   (:name &slot name))
  (:documentation
   "Superclass for UNIX socket connector classes.

    Takes of care of obtaining the `:unix-socket' transport object as
    well as setting up the UNIX socket-specific name options."))

(defmethod shared-initialize :after ((instance   unix-connector)
                                     (slot-names t)
                                     &key
                                     (name nil name-supplied?))
  (when name-supplied?
    (setf (connector-%name instance) name
          (puri:uri-query (slot-value instance 'rsb::uri))
          (format nil "?~A" name))))

(defmethod make-connection-address ((transport unix-socket-transport)
                                    (connector unix-connector))
  (list :name (connector-name connector)))

(defmethod make-connection-options ((transport unix-socket-transport)
                                    (connector unix-connector))
  '())

;;; Direction-specific connector classes

(defclass unix-in-connector (unix-connector
                             in-connector)
  ()
  (:metaclass connector-class))

(register-connector :unix-socket :in 'unix-in-connector)

(defclass unix-out-connector (unix-connector
                              out-connector)
  ()
  (:metaclass connector-class))

(register-connector :unix-socket :out 'unix-out-connector)
