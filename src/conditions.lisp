;;;; conditions.lisp --- Conditions used in cl-rsb.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; RSB-specific errors

(define-condition rsb-error (error)
  ()
  (:documentation
   "This class should be mixed into all RSB-related condition
classes."))

(define-condition communication-error (rsb-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "~@<RSB communication failed.~@:>")))
  (:documentation
   "This error is signaled when some RSB communication fails."))

;;; Participant-related errors

(define-condition participation-failed (rsb-error
                                        chainable-condition)
  ((scope      :initarg  :scope
               :type     (or puri:uri scope)
               :reader   participation-failed-scope
               :documentation
               "The scope of the channel in which the participant
would have participated.")
   (transports :initarg  :transports
               :type     list
               :reader   participation-failed-transports
               :documentation
               "A list of transports the participant would have used
to connect to the bus."))
  (:default-initargs
   :scope      (missing-required-initarg 'participation-failed :scope)
   :transports (missing-required-initarg 'participation-failed :transports))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to participate in the channel ~
                     designated by ~
                     ~S~/rsb::maybe-print-transport-configuration/~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (participation-failed-scope      condition)
             (participation-failed-transports condition)
             condition)))
  (:documentation
   "This error is signaled when the creation of a participant (which
implies participation in a channel) fails."))

(defun maybe-print-transport-configuration (stream transports &optional colon? at?)
  "Print the transport configuration TRANSPORTS to STREAM."
  (declare (ignore colon? at?))
  (format stream "~@[ using transport configuration~{~{~_+ ~@(~A~) ~
                  transport with options~
                  ~&~2T~@<~@;~@{~16S~^: ~S~^, ~_~}~:>~}~}~]"
          transports))

(define-condition listener-creation-failed (participation-failed)
  ()
  (:documentation
   "This error is signaled when an attempt to create a listener
fails."))

(define-condition reader-creation-failed (participation-failed)
  ()
  (:documentation
   "This error is signaled when an attempt to create a reader
fails."))

(define-condition informer-creation-failed (participation-failed)
  ((type :initarg  :type
         :type     (or list symbol)
         :reader   informer-creation-failed-type
         :initform nil
         :documentation
         "The type of the informer for which the creation failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to create RSB informer in the channel ~
                     designated by ~S and type ~
                     ~S~/rsb::maybe-print-transport-configuration/~
                     ~/more-conditions:maybe-print-cause/~@:>"
             (participation-failed-scope      condition)
             (informer-creation-failed-type   condition)
             (participation-failed-transports condition)
             condition)))
  (:documentation
   "This error is signaled when an attempt to create an informer
fails."))

(define-condition no-transports (participation-failed)
  ()
  (:default-initargs
   :transports '())
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to participate in the channel ~
                     designated by ~S because no transports have been ~
                     selected.~@:>"
             (scope-string (participation-failed-scope condition)))))
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
