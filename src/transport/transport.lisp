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
  ((wire-type :initarg  :wire-type
              :reader   transport-wire-type
              :documentation
              "Stores the wire-type of the transport."))
  (:default-initargs
   :wire-type (missing-required-initarg 'transport :wire-type))
  (:documentation
   "Instances of this class represent transport implementations.

    Each transport instance is registered as a provider of the
    `transport' but is also itself a service and the associated
    connectors are providers of that service."))

(defmethod describe-object ((object transport) stream)
  (format stream "~A~
                  ~&Wire-type: ~S~
                  ~@[~2&Connectors:~
                  ~&~{~A~^~&~}~]"
          object
          (transport-wire-type object)
          (service-provider:service-providers object)))

(defmethod service-provider:provider-name ((provider transport))
  (service-provider:service-name provider))

(defmethod service-provider:make-provider
    ((service t) (provider transport)
     &rest args &key
     (direction (missing-required-argument :direction)))
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

(defmethod service-provider:make-provider
    ((service  (eql (service-provider:find-service 'transport)))
     (provider cons)
     &rest args &key)
  (let ((direction (cdr provider)))
    (check-type direction direction "one of :IN-PUSH, :IN-PULL, :OUT")
    (apply #'service-provider:make-provider service (car provider)
           :direction direction
           args)))

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
   direction 'service-provider:class-provider
   `(:class ,class-name)))
