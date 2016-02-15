;;;; transport.lisp --- First class transport objects.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

;;; Constructing a connector instance involves two services and works
;;; roughly like this:
;;;
;;;                    TRANSPORT-SERVICE   TRANSPORT-INSTANCE : `transport'
;;;   (make-provider            │                     │
;;;    'transport               │                     │
;;;    (TRANSPORT . DIRECTION)) │                     │
;;;   ─────────────────────────▷│                     │
;;;                             │ (make-provider      │
;;;                             │  TRANSPORT-INSTANCE │
;;;                             │  DIRECTION)         │
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
                  ~2&Schemas: ~{~S~^, ~}~
                  ~&Wire-type: ~S~
                  ~&Remote: ~S~
                  ~@[~2&Connectors:~
                  ~&~{~A~^~&~}~]"
          object
          (transport-schemas object)
          (transport-wire-type object)
          (transport-remote? object)
          (service-provider:service-providers object)))

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

;;; Transport service

(service-provider:define-service transport
  (:documentation
   "Transports implement possibly networked communication protocols.

    Each transport is implemented by associated connector classes for
    incoming and outgoing communication. The \"directions\" of
    connector classes are :in-push, :in-pull and :out. Instances of
    connector classes of a particular transport are created to
    actually perform communication."))

(defmethod service-provider:find-provider
    ((service  (eql (service-provider:find-service 'transport)))
     (provider symbol)
     &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (or (call-next-method)
      (find provider (service-provider:service-providers service)
            :test #'member :key #'transport-schemas)))

(defmethod service-provider:find-provider
    ((service  (eql (service-provider:find-service 'transport)))
     (provider cons)
     &key if-does-not-exist)
  (let+ (((schema . direction) provider))
    (check-type direction direction "one of :IN-PUSH, :IN-PULL, :OUT")
    (when-let ((provider (service-provider:find-provider
                          service schema
                          :if-does-not-exist if-does-not-exist)))
      (service-provider:find-provider
       provider direction :if-does-not-exist if-does-not-exist))))

(defmethod service-provider:make-provider
    ((service  (eql (service-provider:find-service 'transport)))
     (provider cons)
     &rest args &key)
  (let+ (((schema . direction) provider))
    (check-type direction direction "one of :IN-PUSH, :IN-PULL, :OUT")
    (apply #'call-next-method service provider
           :schema    schema
           :direction direction
           args)))

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
  (service-provider::register-provider
   'transport name transport-class
   (remove-from-plist initargs :transport-class)))

(defun register-connector (transport-name direction class-name)
  (check-type direction direction "one of :IN-PUSH, :IN-PULL, :OUT")
  (service-provider::register-provider
   (service-provider:find-provider 'transport transport-name)
   direction 'connector-provider
   `(:class ,class-name)))
