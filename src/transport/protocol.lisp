;;;; protocol.lisp --- Protocol of the transport module.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

;;; Transport service

(service-provider:define-service transport
  (:documentation
   "Transports implement possibly networked communication protocols.

    Each transport is implemented by associated connector classes for
    incoming and outgoing communication. The \"directions\" of
    connector classes are :in and :out. Instances of connector classes
    of a particular transport are created to actually perform
    communication."))

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
    (check-type direction direction "one of :IN, :OUT")
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
    (check-type direction direction "one of :IN, :OUT")
    (apply #'call-next-method service provider :schema schema args)))

;;; Transport protocol

(defgeneric transport-schemas (transport)
  (:documentation
   "Return a list of the (URI-)schemas supported by TRANSPORT.

    TRANSPORT can be a symbol designating a transport, a transport
    object, a connector class or a connector instance."))

(defgeneric transport-wire-type (transport)
  (:documentation
   "Return the wire-type of TRANSPORT.

    TRANSPORT can be a symbol designating a transport, a transport
    object, a connector class or a connector instance."))

(defgeneric transport-remote? (transport)
  (:documentation
   "Return true if TRANSPORT implements remote communication."))

;; Default behavior

(macrolet ((define-transport-accessor (name)
             `(defmethod ,name ((transport symbol))
                ;; When given a symbol, try to look up the designated
                ;; transport and retrieving the requested slot value
                ;; from it.
                (,name (service-provider:find-provider 'transport transport)))))
  (define-transport-accessor transport-schemas)
  (define-transport-accessor transport-wire-type)
  (define-transport-accessor transport-remote?))

;;; Connector protocol

(defgeneric connector-url (connector)
  (:documentation
   "Return a base URL that can be used to locate resources via
    CONNECTOR."))

(defgeneric connector-relative-url (connector thing)
  (:documentation
   "Return a complete URL suitable for locating the resource THING via
    CONNECTOR."))

;;; Connector introspection protocol

(defgeneric connector-transport (connector)
  (:documentation
   "Return the transport of CONNECTOR.

    CONNECTOR can be a connector class or a connector instance."))

(defgeneric connector-direction (connector)
  (:documentation
   "Return the communication direction of CONNECTOR.

    CONNECTOR can be a connector class or a connector instance."))

(defgeneric connector-direct-options (connector)
  (:documentation
   "Return a description of the options defined in CONNECTOR.

    The returned description is a list of \"direct\" options, i.e.
    defined in CONNECTOR but not its superclasses. Items are of the
    form

      (NAME TYPE &key DEFAULT DESCRIPTION)

    where NAME is a keyword which names the option, TYPE is the type
    of acceptable values of the option, DEFAULT is a form that
    computes a default value and description is a description of the
    option.

    CONNECTOR can be a connector class or a connector instance."))

(defgeneric connector-options (connector)
  (:documentation
   "Return a description of the options accepted by CONNECTOR.

    The returned description is a list of \"transitive\" options,
    i.e. accumulated along superclasses. Items are of the form

      (NAME TYPE &key DEFAULT DESCRIPTION)

    where NAME is a keyword which names the option, TYPE is the type
    of acceptable values of the option, DEFAULT is a form that
    computes a default value and description is a description of the
    option.

    CONNECTOR can be a connector class or a connector instance."))

;;; Notification receiver protocol

(defgeneric receive-notification (connector block?)
  (:documentation
   "Receive and return one notification via CONNECTOR.

If BLOCK? is nil, only return a notification if one is immediately
available, otherwise return nil. If BLOCK? is non-nil, wait for a
notification if none is immediately available.

If something has been received, return two values: the received
notification and a symbol designating the wire-schema of the received
data."))

(defgeneric notification->event (connector notification wire-schema)
  (:documentation
   "Convert NOTIFICATION with wire-schema WIRE-SCHEMA into an `event'
instance and return the event. If NOTIFICATION cannot be converted
into an event, return nil instead. Signal a `decoding-error' if
something goes wrong."))

;;; Notification sender protocol

(defgeneric send-notification (connector notification)
  (:documentation
   "Send NOTIFICATION via CONNECTOR."))

(defgeneric event->notification (connector event)
  (:documentation
   "Convert EVENT into a notification for sending via
CONNECTOR. Return the notification. If EVENT cannot be converted into
a notification, maybe return nil, depending on the error handling
policy. Maybe signal an `encoding-error' if something goes wrong."))

;;; Threaded receiver protocol

(defgeneric start-receiver (connector)
  (:documentation
   "Ask CONNECTOR to start a receiver thread that runs
`receive-messages' until interrupted."))

(defgeneric stop-receiver (connector)
  (:documentation
   "Ask CONNECTOR to stop receiving messages."))

(defgeneric receive-messages (connector)
  (:documentation
   "CONNECTOR receives and processes messages until interrupted."))

;;; Transport metric exposing protocol

(defgeneric connector-expose (connector)
  (:documentation
   "Return the list of transport metrics exposed by CONNECTOR."))

(defgeneric (setf connector-expose) (new-value connector)
  (:documentation
   "Install NEW-VALUE as the list of transport metrics exposed by
CONNECTOR."))

(defgeneric connector-expose? (connector metric)
  (:documentation
   "Return non-nil when CONNECTOR exposes METRIC."))

(defgeneric (setf connector-expose?) (new-value connector metric)
  (:documentation
   "If NEW-VALUE is non-nil, add METRIC to the list of transport
metrics exposed by CONNECTOR. Otherwise remove METRIC from the
list."))

;;; Transport implementations

(defun make-connector (name direction converters
                       &rest args &key converter &allow-other-keys)
  "Create a connector instance for the direction designated by
   DIRECTION of the kind the designated by NAME. Pass ARGS to the
   constructed instance.

   CONVERTERS is an alist of items of the form

     (WIRE-TYPE . CONVERTER)

   . If the requested connector does not require a converter,
   CONVERTERS can be nil.

   An error of type `connector-constructor-failed' is signaled if the
   requested connector instance cannot be constructed. If the
   construction fails due to the lack of a suitable converter, an
   error of the subtype `no-suitable-converter' is signaled."
  (with-condition-translation
      (((error connector-construction-failed)
        :name      name
        :direction direction
        :args      args))
    (let+ ((wire-type (transport-wire-type name))
           (converter (unless (eq wire-type t)
                        (or converter
                            (cdr (find wire-type converters
                                       :key  #'car
                                       :test #'subtypep)))))
           (args      (remove-from-plist args :converter))
           ((&flet make-it (&rest more-args)
              (apply #'service-provider:make-provider
                     'transport (cons name direction)
                     (append args more-args)))))
      (cond
        ;; The connector does not require a converter.
        ((eq wire-type t)
         (make-it))
        ;; The connector requires a converter and we found a suitable
        ;; one.
        (converter
         (make-it :converter converter))
        ;; The connector requires a converter, but we did not find a
        ;; suitable one.
        (t
         (error 'no-suitable-converter
                :name       name
                :direction  direction
                :args       args
                :wire-type  wire-type
                :candidates converters))))))

(defun make-connectors (specs direction &optional converters)
  "Create and return zero or more connector instances for the
   direction designated by DIRECTION according to SPECS.

   Each element of SPECS has to be of the form

     (NAME . ARGS)

   where NAME and ARGS have to acceptable for calls to
   `make-connector'. For CONVERTERS, see `make-connector'."
  ;; Check direction here in order to signal a appropriate type error
  ;; even if SPECS is nil.
  (check-type direction direction "one of :IN-PUSH, :IN-PULL, :OUT")

  (iter (for (name . args) in specs)
        (collect (apply #'make-connector
                        name direction converters args))))
