;;;; transport.lisp --- First class transport objects.
;;;;
;;;; Copyright (C) 2015, 2016, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

;;; Constructing a connector instance involves two services and works
;;; roughly like this:
;;;
;;;                    TRANSPORT-SERVICE   TRANSPORT-INSTANCE : `transport'
;;;   (make-provider            │                     │
;;;    'transport               │                     │
;;;    (SCHEMA . DIRECTION))    │                     │
;;;   ─────────────────────────▷│                     │
;;;                             │ (make-provider      │
;;;                             │  TRANSPORT-INSTANCE │
;;;                             │  DIRECTION          │
;;;                             │  :schema SCHEMA)    │
;;;                             │────────────────────▷│
;;;                             │                     │
;;;                             │                     │──▷CONNECTOR-INSTANCE
;;;                             │                     │            │
;;;                             │                     │            │
;;;
;;; Note that TRANSPORT-INSTANCE is an instance of the `transport'
;;; class and acts as
;;;
;;; 1) a provider of TRANSPORT-SERVICE (the service
;;;    registered under the name 'transport)
;;;
;;; 2) a service the providers of which are connectors for different
;;;    directions (i.e. :in-pull, :in-pull and :out)

;;; `transport' class

(defclass transport (service-provider:standard-service)
  ((schemas   :type     list
              :reader   transport-schemas
              :accessor transport-%schemas
              :documentation
              "Stores a list of schemas supported by the transport.")
   (wire-type :initarg  :wire-type
              :reader   transport-wire-type
              :documentation
              "Stores the wire-type of the transport.")
   (remote?   :initarg  :remote?
              :reader   transport-remote?
              :initform t
              :documentation
              "True if the transport implements remote
               communication."))
  (:default-initargs
   :schemas   (missing-required-initarg 'transport :schemas)
   :wire-type (missing-required-initarg 'transport :wire-type))
  (:documentation
   "Instances of this class represent transport implementations.

    Each transport instance is registered as a provider of the
    `transport' but is also itself a service and the associated
    connectors are providers of that service."))

(defmethod shared-initialize :after ((instance   transport)
                                     (slot-names t)
                                     &key
                                     (schemas nil schemas-supplied?))
  (when schemas-supplied?
    (setf (transport-%schemas instance) (ensure-list schemas))))

(defmethod print-items:print-items append ((object transport))
  `((:remote? ,(transport-remote? object) " ~:[local~:;remote~]"
              ((:after :provider-count)))))

(defmethod describe-object ((object transport) stream)
  (format stream "~A~
                  ~2&Schemas:   ~{~S~^, ~}~
                  ~&Wire-type: ~S~
                  ~&Remote:    ~S~
                  ~@[~2&Connectors:~
                  ~&~{~A~^~&~}~]~
                  ~@[~2&Documentation:~&~A~]"
          object
          (transport-schemas object)
          (transport-wire-type object)
          (transport-remote? object)
          (service-provider:service-providers object)
          (documentation object t)))

(defmethod service-provider:provider-name ((provider transport))
  (service-provider:service-name provider))

(defmethod service-provider:make-provider
    ((service t) (provider transport)
     &rest args &key
     (schema    (missing-required-argument :schema))
     (direction (missing-required-argument :direction)))
  (declare (ignore schema))
  ;; PROVIDER is also a service:
  (apply #'service-provider:make-provider provider direction
         (remove-from-plist args :direction)))

;;; `connector-provider'

(defclass connector-provider (service-provider:class-provider)
  ()
  (:documentation
   "Provider class for connector classes."))

(defmethod shared-initialize :after ((instance   connector-provider)
                                     (slot-names t)
                                     &key)
  (closer-mop:finalize-inheritance
   (service-provider:provider-class instance)))

;;; Registration

(defun register-transport (name &rest initargs
                           &key
                           (transport-class 'transport)
                           &allow-other-keys)
  (apply #'service-provider:register-provider
         'transport name transport-class
         (remove-from-plist initargs :transport-class)))

(defun register-connector (transport-name direction class-name)
  (check-type direction direction "one of :IN-PUSH, :IN-PULL, :OUT")
  (service-provider:register-provider
   (service-provider:find-provider 'transport transport-name)
   direction 'connector-provider :class class-name))
