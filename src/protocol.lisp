;;;; protocol.lisp --- Main client-facing protocol provided by cl-rsb.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Error handling

(defun retry ()
  "Invoke the `retry' restart."
  (if-let ((restart (find-restart 'retry)))
    (invoke-restart restart)
    (warn "~@<Restart ~S not found; Doing nothing.~@:>" 'retry)))

;; use-value restart and function are provided by CL.

;; continue restart and function are provided by CL.

;; abort restart and function are provided by CL.

;;; Scope protocol

(defgeneric scope-components (scope)
  (:documentation
   "Return a list containing the components of SCOPE."))

(defgeneric scope-string (scope)
  (:documentation
   "Return a string representation of SCOPE."))

(defgeneric make-scope (thing
                        &key
                        intern?)
  (:documentation
   "Parse string and return a `scope' instance."))

(declaim (ftype (function (scope) (values scope &optional)) intern-scope))

;;; Event protocol

(defgeneric event-id/opaque (event)
  (:documentation
   "Return an object that uniquely identifies EVENT."))

;;; Component URL protocol

(defgeneric relative-url (component)
  (:documentation
   "Return a relative URL that allows to locate COMPONENT when
anchored at an absolute location like a transport URL."))

(defgeneric abstract-uri (component)
  (:documentation
   "Return a URI with \"rsb\" scheme, no host and port information
merged with the relative URL of COMPONENT."))

(defgeneric transport-specific-urls (component)
  (:documentation
   "Return a list of URLs each of which describes the location and/or
reachability of COMPONENT in transport-specific way."))

;; Default behavior

(defmethod abstract-uri ((component t))
  "Merge the relative URL of COMPONENT with a scheme-only \"anchor\"."
  (puri:merge-uris (relative-url component)
                   (make-instance 'puri:uri
                                  :scheme :rsb)))

(defmethod transport-specific-urls ((component t))
  "For arbitrary COMPONENTs, location information is not available."
  '())

;;; Common participant protocol

(defgeneric participant-kind (participant)
  (:documentation
   "Return a keyword designating the kind of PARTICIPANT."))

(defgeneric participant-id (participant)
  (:documentation
   "Return the unique id of PARTICIPANT."))

(defgeneric participant-scope (participant)
  (:documentation
   "Return the scope of the channel in which PARTICIPANT
    participates."))

(defgeneric participant-converter (participant wire-type
                                   &key
                                   error?)
  (:documentation
   "Return the converter used by PARTICIPANT for WIRE-TYPE.
    If PARTICIPANT does not have a converter for WIRE-TYPE, signal an
    error if ERROR? is non-nil, otherwise return nil."))

(defgeneric participant-error-hook (participant)
  (:documentation
   "Return the error hook of PARTICIPANT. Handlers attached to the
    returned hook are called when errors are signaled in PARTICIPANT
    or an associated object."))

(defgeneric detach (participant)
  (:documentation
   "Detach PARTICIPANT from the channel in which it participates and
    the transport or transports it uses for this participation."))

(defun detach/ignore-errors (participant)
  "Like `detach' but handle errors that occur during detaching by
   continuing in a best effort manner instead of signaling."
  (handler-bind
      (((or error bt:timeout)
        (lambda (condition)
          (warn "~@<Error during detaching of ~A: ~A~@:>"
                participant condition)
          (continue))))
    (detach participant)))

;; Default behavior

(defmethod participant-kind ((participant t))
  (values (make-keyword (type-of participant))))

(defmethod participant-converter :around ((participant t) (wire-type t)
                                          &key
                                          (error? t))
  "Signal an error if the next method could not retrieve the
converter."
  (or (call-next-method)
      (when error?
        (error "~@<Participant ~A does not have a converter for ~
                wire-type ~A.~@:>"
               participant wire-type))))

;;; Participant creation protocol
;;;
;;; 1. Clients of the protocol call `make-participant' to request the
;;;    creation of an instance of a given participant class.
;;;
;;;    Methods on this generic function are in charge of:
;;;    * Establishing appropriate restarts
;;;    * Translating error conditions into
;;;      `participant-creation-error' conditions
;;;    * Parsing string designators or URIs and scopes
;;;    * Splitting URIs into scopes and transport options
;;;
;;; 2. The specified class is located and a prototype instance is
;;;    obtained (e.g. via `class-prototype').
;;;
;;; 3. `make-participant-using-class' is called with arguments
;;;    augmented with the prototype instance.
;;;
;;;    This allows methods on `make-participant-using-class' to
;;;    customize participant creation for certain classes and their
;;;    respective subclasses.

(defgeneric make-participant (class scope &rest args
                              &key
                              direction
                              transports
                              converters
                              transform
                              error-policy
                              &allow-other-keys)
  (:documentation
   "Make and return a participant instance of CLASS that participates
    in the channel designated by SCOPE.

    In general, the keyword arguments ARGS are used as initargs for
    the created CLASS instance. Some commonly accepted initargs are
    described below.

    All participant classes accept the keyword parameter

      ERROR-POLICY has to be nil or a function to be installed in the
      error hook of the created participant.

    Participant classes which directly send or receive
    events (e.g. `reader', `listener' and `informer') support the
    following keyword parameters:

      TRANSPORTS is a list of connector classes.

      CONVERTERS is an alist of converters for particular wire-types
      with items of the form (WIRE-TYPE . CONVERTER).

    Many, but not necessarily all, participant classes support the
    keyword parameter

      TRANSFORM, when non-nil, is a transform object usable with
      `rsb.event-processing:transform!'.

    Return the `participant' (more likely a subclass thereof)
    instance."))

(defgeneric make-participant-using-class (class prototype scope
                                          &rest args &key &allow-other-keys)
  (:documentation
   "Make and return a participant instance of CLASS, optionally using
    PROTOTYPE for dispatch.

    PROTOTYPE is an instance of CLASS intended for dispatch (it might
    not be properly initialized but is guaranteed to be an instance of
    CLASS).

    See `make-participant' for a description of the remainder of the
    parameters."))

;; Default behavior

(defclass scope () ())
(defmethod make-participant-using-class ((class     class)
                                         (prototype t)
                                         (scope     scope)
                                         &rest args &key)
  (apply #'make-instance class :scope scope args))

;;; Common protocol for receiving participants

(defgeneric receiver-filters (receiver)
  (:documentation
   "Return the list of filters associated to the receiving participant
RECEIVER."))

(defgeneric (setf receiver-filters) (new-value receiver)
  (:documentation
   "Set the list of filters associated to the receiving participant
RECEIVER to NEW-VALUE."))

;;; Listener protocol

(defgeneric make-listener (scope-or-uri
                           &key
                           transports
                           converters
                           transform
                           error-policy)
  (:documentation
   "Listen to events on the channel designated by SCOPE-OR-URI.
If successful return a `listener' instance. Otherwise an error of type
`listener-creation-error' is signaled.

TRANSPORTS determines the transport configuration that is used to
participate in the channel. See `rsb.transport:make-connectors' for
details regarding acceptable values of TRANSPORTS.

CONVERTERS, if supplied, is an list that specifies a set of converters
for particular wire-types from which the converters that are used in
transports should be chosen. Items are of the form (WIRE-TYPE
. CONVERTER). If CONVERTERS is not supplied, a default set of
converters is derived from the default configuration.

When non-nil, TRANSFORM is a transform object, usable with
`rsb.event-processing:transform!', that should be applied to received
events.

ERROR-POLICY has to be nil or a function to be installed in the error
hook of the created `listener'."))

;;; Reader protocol

(defgeneric make-reader (scope-or-uri
                         &key
                         transports
                         converters
                         transform
                         error-policy)
  (:documentation
   "Receive events on the channel designated by SCOPE-OR-URI.
If successful, return a `reader' instance. Otherwise an error of type
`reader-creation-error' is signaled.

TRANSPORTS determines the transport configuration that is used to
participate in the channel. See `rsb.transport:make-connectors' for
details regarding acceptable values of TRANSPORTS.

CONVERTERS, if supplied, is an list that specifies a set of converters
for particular wire-types from which the converters that are used in
transports should be chosen. Items are of the form (WIRE-TYPE
. CONVERTER). If CONVERTERS is not supplied, a default set of
converters is derived from the default configuration.

When non-nil, TRANSFORM is a transform object, usable with
`rsb.event-processing:transform!', that should be applied to received
events.

ERROR-POLICY has to be nil or a function to be installed in the error
hook of the created `reader'.

The resulting `reader' instance can be used to receive data in
\"pull\" manner using the `receive' function."))

(defgeneric receive (reader
                     &key
                     block?)
  (:documentation
   "Receive data from the channel in which READER is
participating. When data is received, it is returned in form of an
`event' instance. If BLOCK? is non-nil, wait for data to become
available if there is none. If BLOCK? is nil and no data is available,
nil is returned."))

;;; Informer protocol

(defgeneric make-informer (scope-or-uri type
                           &key
                           transports
                           converters
                           transform
                           error-policy)
  (:documentation
   "Start publishing data of type TYPE on the channel designated by
SCOPE-OR-URI. If successful, return an `informer' instance. Otherwise
an error of type `informer-creation-error' is signaled.

TRANSPORTS determines the transport configuration that is used to
participate in the channel. See `rsb.transport:make-connectors' for
details regarding acceptable values of TRANSPORTS.

CONVERTERS, if supplied, is an list that specifies a set of converters
for particular wire-types from which the converters that are used in
transports should be chosen. Items are of the form (WIRE-TYPE
. CONVERTER). If CONVERTERS is not supplied, a default set of
converters is derived from the default configuration.

When non-nil, TRANSFORM is a transform object, usable with
`rsb.event-processing:transform!', that should be applied to sent
events.

ERROR-POLICY has to be nil or a function to be installed in the error
hook of the created `informer'."))

(defgeneric send (informer data
                  &rest meta-data
                  &key
                  method
                  timestamps
                  causes
                  unchecked?
                  &allow-other-keys)
  (:documentation
   "Send DATA to participants of the channel in which INFORMER
participates. Add key-value pairs in META-DATA to the meta-data of the
event created from DATA.

METHOD can be used to set the method of the sent event.

TIMESTAMPS, if supplied, is interpreted as a plist of named timestamps
in which values are `local-time:timestamp' instances.

CAUSES can be used to add a list of cause vectors to the sent
event. The cause vector have to be `event-id's.

UNCHECKED? controls whether the compatibility of INFORMER and DATA
should be validated. This mainly applies if data is an `event'
instance since properties like the scope of the event may conflict
with properties of INFORMER."))
