;;;; protocol.lisp --- Main client-facing protocol provided by rsb.
;;;;
;;;; Copyright (C) 2011-2016, 2018, 2019 Jan Moringen
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

;; intern-scope

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

(defclass participant () ()) ; forward declaration
(declaim (special *participant-state-change-hook*))

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

(defmethod detach :around ((participant participant))
  (if *participant-state-change-hook*
      (unwind-protect
           (call-next-method)
        (hooks:run-hook '*participant-state-change-hook*
                        participant :detached))
      (call-next-method)))

;;; Participant creation protocol
;;;
;;; 1. Clients of the protocol call `make-participant' to request the
;;;    creation of an instance of a given participant kind.
;;;
;;;    Methods on this generic function are in charge of:
;;;    * Establishing appropriate restarts
;;;    * Translating error conditions into
;;;      `participant-creation-error' conditions
;;;    * Parsing string designators or URIs and scopes
;;;    * Splitting URIs into scopes and transport options
;;;
;;; 2. `service-provider:make-provider' is used to locate the
;;;    requested participant class and obtain a prototype instance
;;;    (e.g. via. `class-prototype').
;;;
;;;    * In most cases, this is achieved by registering instances of
;;;      the specialized `participant-provider' provider class
;;;
;;; 3. Normally, the provider calls `make-participant-using-class'
;;;    with arguments augmented with the prototype instance.
;;;
;;;    This allows methods on `make-participant-using-class' to
;;;    customize participant creation for certain classes and their
;;;    respective subclasses.

(defgeneric make-participant (kind scope &rest args
                              &key
                              transports
                              converters
                              transform
                              error-policy
                              parent
                              introspection?
                              &allow-other-keys)
  (:documentation
   "Make and return a participant instance of KIND that participates
    in the channel designated by SCOPE.

    In general, the keyword arguments ARGS are used as initargs for
    the created CLASS instance. Some commonly accepted initargs are
    described below.

    All participant classes accept the keyword parameters

      ERROR-POLICY has to be nil or a function to be installed in the
      error hook of the created participant.

      PARENT, if supplied, is a participant that should be considered
      the parent of the created participant.

      INTROSPECTION? controls whether the created participant
      participates in the introspection machinery. Specifically,
      whether it announces its creation and destruction and answers to
      introspection queries.

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
    instance.

    If the participant cannot be created, an error of type
    `participant-creation-error' is signaled."))

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

;; Service

(defclass participant-provider (service-provider:class-provider)
  ()
  (:documentation
   "Provider class which implements `service-provider:make-provider'
    by delegating to `make-participant-using-class'."))

(defmethod shared-initialize :after ((instance   participant-provider)
                                     (slot-names t)
                                     &key)
  (closer-mop:finalize-inheritance
   (service-provider:provider-class instance)))

(defmethod service-provider:make-provider ((service  t)
                                           (provider participant-provider)
                                           &rest args)
  (let* ((class     (service-provider:provider-class provider))
         (prototype (closer-mop:class-prototype class)))
    (apply #'make-participant-using-class class prototype args)))

(service-provider:define-service participant
  (:documentation
   "Providers of this service are classes conforming to the
    participant protocol."))

(defun register-participant-class (class
                                   &optional
                                   (designator (make-keyword class)))
  (service-provider:register-provider/class
   'participant designator
   :class          class
   :provider-class 'participant-provider))

;; Default behavior

(defclass scope () ()) ; forward declarations
(declaim (special *make-participant-hook*))

;; The variable `*make-participant-nesting*' is used to distinguish
;; "distinct" recursive calls to `make-participant' in order to
;; perform accurate condition translation.
;;
;; For example, if `make-participant' is called with a scope
;; designated by a string, the :around method establishes a condition
;; translating handler. Next, there is a recursive call to
;; `make-participant' with the parsed scope. This should not be
;; treated as a distinct call meaning that errors signaled in the
;; inner call should not be wrapped in multiple
;; `participant-creation-error' conditions. However, if the creation
;; of the requested participant involves creating another participant,
;; and the nested creation signals an, there should be a chain of
;; `participant-creation-error' conditions, one for each requested
;; participant.
(declaim (type (cons boolean integer) *make-participant-nesting*))
(defvar *make-participant-nesting* (cons nil 0))

(defmethod make-participant :around ((kind t) (scope t)
                                     &key
                                     transports)
  ;; Translate different kinds of errors into
  ;; `participant-creation-error' errors.
  (if (car *make-participant-nesting*)
      (call-next-method)
      (let* ((depth                      (1+ (cdr *make-participant-nesting*)))
             (*make-participant-nesting* (cons t depth)))
        (handler-bind
            ((error (lambda (condition)
                      (when (or (/= depth (cdr *make-participant-nesting*))
                                (not (typep condition 'participant-creation-error)))
                        (error 'participant-creation-error
                               :kind       kind
                               :scope      scope
                               :transports transports ; TODO not always available
                               :cause      condition)))))
          (call-next-method)))))

(defmethod make-participant
    ((kind t) (scope scope)
     &rest args &key
     (introspection? (when (member (option-value '(:introspection :enabled)) '(t "1")
                                   :test #'equal)
                       t)))
  (let* ((*make-participant-nesting*
          (cons nil (cdr *make-participant-nesting*)))
         (participant (apply #'service-provider:make-provider
                             'participant kind scope args)))
    (if *make-participant-hook*
        ;; When the hook returns anything but nil, use it as
        ;; replacement for PARTICIPANT. Otherwise use PARTICIPANT.
        (or (hooks:run-hook
             '*make-participant-hook* participant
             (list* :introspection? introspection?
                    (remove-from-plist args :introspection?)))
            participant)
        participant)))

(defmethod make-participant-using-class ((class     class)
                                         (prototype t)
                                         (scope     scope)
                                         &rest args &key)
  (apply #'make-instance class :scope scope
         (remove-from-plist args :parent :introspection?)))

;;; Common protocol for receiving participants

(defgeneric receiver-filters (receiver)
  (:documentation
   "Return the list of filters associated to the receiving participant
RECEIVER."))

(defgeneric (setf receiver-filters) (new-value receiver)
  (:documentation
   "Set the list of filters associated to the receiving participant
RECEIVER to NEW-VALUE."))

;;; Informer protocol

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

;;; Timed executor protocol

(defgeneric executor-interval (executor)
  (:documentation
   "Return the interval in seconds between calls to EXECUTOR's task
    function."))

(defgeneric (setf executor-interval) (new-value executor)
  (:documentation
   "Set the interval in seconds between calls to EXECTUOR's task
    function to NEW-VALUE."))
