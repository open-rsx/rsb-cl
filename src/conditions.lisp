;;;; conditions.lisp --- Conditions used in cl-rsb.
;;;;
;;;; Copyright (C) 2010-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Generic RSB conditions

(define-condition rsb-condition (condition)
  ()
  (:documentation
   "This class should be mixed into all RSB-related condition
    classes."))

(define-condition rsb-problem-condition (rsb-condition)
  ()
  (:documentation
   "This class should be mixed into all RSB-related problem
    condition (i.e. warnings and errors) classes."))

(define-condition rsb-warning (warning
                               rsb-problem-condition)
  ()
  (:documentation
   "This class should be mixed into all RSB-related warning condition
    classes."))

(define-condition rsb-error (error
                             rsb-problem-condition)
  ()
  (:documentation
   "This class should be mixed into all RSB-related error condition
    classes."))

;;; Scope-related conditions

(define-condition scope-parse-error (parse-error
                                     rsb-error
                                     simple-condition)
  ((string   :initarg :string
             :type    string
             :reader  scope-parse-error-string
             :documentation
             "Stores the invalid scope string.")
   (position :initarg :position
             :type    array-index
             :reader  scope-parse-error-position
             :documentation
             "Stores the position of the first invalid character."))
  (:default-initargs
   :string   (missing-required-initarg 'scope-parse-error :string)
   :position (missing-required-initarg 'scope-parse-error :position))
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o scope-parse-error- string position) condition))
       (format stream "~@<The string~@:_~
                       ~@:_~
                       ~2@T~A~@:_~
                       ~2@T~V@T^~@:_~
                       ~@:_~
                       could not be parsed as a scope~
                       ~/more-conditions:maybe-print-explanation/~@:>"
               string position condition))))
  (:documentation
   "This error is signaled when string cannot be parsed as a scope."))

(defun scope-parse-error (string position
                          &optional format-control &rest format-arguments)
  (error 'scope-parse-error
         :string           string
         :position         position
         :format-control   format-control
         :format-arguments format-arguments))

;;; Communication related conditions

(define-condition communication-error (rsb-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<RSB communication failed.~@:>")))
  (:documentation
   "This error is signaled when some RSB communication fails."))

;;; Participant-related errors

(define-condition participant-creation-error (rsb-error
                                              chainable-condition)
  ((kind       :initarg  :kind
               :type     keyword
               :reader   participant-creation-error-kind
               :documentation
               "Kind of the participant the creation of which failed.")
   (scope      :initarg  :scope
               :type     (or string puri:uri scope)
               :reader   participant-creation-error-scope
               :documentation
               "The scope of the channel in which the participant
                would have participated.")
   (transports :initarg  :transports
               :type     list
               :reader   participant-creation-error-transports
               :documentation
               "A list of transports the participant would have used
                to connect to the bus."))
  (:default-initargs
   :kind       (missing-required-initarg 'participant-creation-error :kind)
   :scope      (missing-required-initarg 'participant-creation-error :scope)
   :transports (missing-required-initarg 'participant-creation-error :transports))
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o
              participant-creation-error- kind scope transports)
             condition))
       (format stream "~@<Failed to create ~A participant in the channel ~
                       designated by ~
                       ~S~/rsb::maybe-print-transport-configuration/~
                       .~/more-conditions:maybe-print-cause/~@:>"
               kind (etypecase scope
                      (string   scope)
                      (puri:uri (princ-to-string scope))
                      (scope    (scope-string scope)))
               transports condition))))
  (:documentation
   "This error is signaled when the creation of a participant (which
    implies participation in a channel) fails."))

(defun maybe-print-transport-configuration (stream transports
                                            &optional colon? at?)
  "Print the transport configuration TRANSPORTS to STREAM."
  (declare (ignore colon? at?))
  (format stream "~@[ using transport configuration~@:_~@:_~
                    ~{~{~
                      + ~@(~A~) transport~^ with options~@:_~@:_~
                        ~2@T~@<~@;~@{~16S~^ ~S~^~@:_~}~:>~
                    ~}~^~@:_~@:_~}~
                  ~@:_~@:_~]"
          transports))

(define-condition no-transports-error (participant-creation-error)
  ()
  (:default-initargs
   :transports '())
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to create ~A participant in the channel ~
                     designated by ~S because no transports have been ~
                     selected.~@:>"
             (participant-creation-error-kind condition)
             (scope-string (participant-creation-error-scope condition)))))
  (:documentation
   "This error is signaled when the creation of a participant fails
    because an empty list of transports is supplied."))

;;; Event sending conditions

(define-condition event-error (rsb-error)
  ((event :initarg  :event
          :reader   event-error-event
          :documentation
          "The invalid event."))
  (:default-initargs
   :event (missing-required-initarg 'event-error :event))
  (:report
   (lambda (condition stream)
     (format stream "~@<The event ~S was used in a context for which ~
                     it is not valid.~:@>"
             (event-error-event condition))))
  (:documentation
   "This error and its subclasses are signaled when an attempt is made
    to use an event in a context for which it is not valid."))

(define-condition event-type-error (type-error
                                    event-error)
  ()
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o type-error- datum expected-type) condition))
       (format stream "~@<The type ~S of event ~S is not ~S.~@:>"
               (type-of (event-data datum)) datum expected-type))))
  (:documentation
   "This error is signaled when an event is used in a context in which
    its type is unsuitable."))

(define-condition event-scope-error (event-error)
  ((expected-scope :initarg    :expected-scope
                   :type       scope
                   :reader     event-error-expected-scope
                   :documentation
                   "The scope the invalid event was expected to have."))
  (:default-initargs
   :expected-scope (missing-required-initarg 'event-scope-error :expected-scope))
  (:report
   (lambda (condition stream)
     (let+ (((&structure-r/o event-error- event expected-scope) condition))
      (format stream "~@<The scope ~A of the event ~S is not identical ~
                     to or a sub-scope of the expected scope ~A.~:@>"
              (event-scope event) event expected-scope))))
  (:documentation
   "This error is signaled when an event is used in a context in which
    its scope is not valid."))
