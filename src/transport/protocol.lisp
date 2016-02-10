;;;; protocol.lisp --- Protocol of the transport module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

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

(defgeneric connector-direction (connector)
  (:documentation
   "Return the communication direction of CONNECTOR.

    CONNECTOR can be a connector class or a connector instance."))

(defgeneric connector-wire-type (connector)
  (:documentation
   "Return the wire-type of CONNECTOR.

    CONNECTOR can be a connector class or a connector instance."))

(defgeneric connector-schemas (connector)
  (:documentation
   "Return a list of the (URI-)schemas supported by CONNECTOR.

    CONNECTOR can be a connector class or a connector instance."))

(defgeneric connector-options (connector)
  (:documentation
   "Return a description of the options accepted connector.

    The returned description is a list of items of the form

      (NAME TYPE &optional DOCUMENTATION)

    where NAME is a keyword which names the option and TYPE is the
    type of acceptable values of the option.

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

(dynamic-classes:define-findable-class-family transport
    "Transports are implemented by input and output connector
classes. These are designated by names of the
form :TRANSPORT-in-push, :TRANSPORT-in-pull and :TRANSPORT-out
respectively.")

(defun find-connector-class (name direction)
  "Return the connector class designated by NAME for the direction
designated by DIRECTION. DIRECTION has to be one of :in-push, :in-pull
and :out. An error of type `no-such-transport-class' is signaled if
the requested class cannot be found."
  (check-type name      keyword   "a keyword")
  (check-type direction direction "either :IN-PUSH, :IN-PULL or :OUT")

  (find-transport-class
   (format-symbol :keyword "~A-~A" name direction)))

(defun make-connector (name direction converters &rest args)
  "Create a connector instance for the direction designated by
DIRECTION of the kind the designated by NAME. Pass ARGS to the
constructed instance.
CONVERTERS is an alist of items of the form (WIRE-TYPE
. CONVERTER). If the requested connector does not require a converter,
CONVERTERS can be nil.
An error of type `connector-constructor-failed' is signaled if the
requested connector instance cannot be constructed. If the
construction fails due to the lack of a suitable converter, an error
of the subtype `no-suitable-converter' is signaled."
  (handler-bind
      (((and error (not no-suitable-converter))
        (lambda (condition)
          (error 'connector-construction-failed
                 :name      name
                 :direction direction
                 :args      args
                 :cause     condition))))
    (let* ((class     (find-connector-class name direction))
           (wire-type (connector-wire-type class))
           (converter (unless (eq wire-type t)
                        (or (getf args :converter)
                            (cdr (find wire-type converters
                                       :key  #'car
                                       :test #'subtypep)))))
           (args      (remove-from-plist args :converter)))
      (cond
        ;; The connector does not require a converter.
        ((eq wire-type t)
         (apply #'make-instance class
                :schema name
                args))
        ;; The connector requires a converter and we found a suitable
        ;; one.
        (converter
         (apply #'make-instance class
                :schema    name
                :converter converter
                args))
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
direction designated by DIRECTION according to SPECS. Each element of
SPECS has to be of the form

  (NAME . ARGS)

where NAME and ARGS have to acceptable for calls to
`make-connector'. For CONVERTERS, see `make-connector'."
  ;; Check direction here in order to signal a appropriate type error
  ;; even if SPECS is nil.
  (check-type direction direction "either :IN-PUSH, :IN-PULL or :OUT")

  (iter (for (name . args) in specs)
        (collect (apply #'make-connector
                        name direction converters args))))
