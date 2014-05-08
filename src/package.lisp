;;;; package.lisp --- Main package definition for the rsb system.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:iterate
   #:let-plus
   #:hooks
   #:more-conditions)

  ;; Types
  (:export
   #:scope-component-character #:scope-component-character?
   #:scope-component           #:scope-component?
   #:scope-components          #:scope-components?
   #:scope-designator

   #:derive-scope-component

   #:sequence-number
   #:timestamp-designator
   #:meta-data-designator

   #:error-policy
   #:timeout

   #:implementation-feedback
   #:direction
   #:wire-type)

  ;; Conditions
  (:export
   #:rsb-condition
   #:rsb-problem-condition
   #:rsb-warning
   #:rsb-error

   #:communication-error

   #:participant-creation-error
   #:participant-creation-error-kind
   #:participant-creation-error-scope
   #:participant-creation-error-transports

   #:listener-creation-error

   #:reader-creation-error

   #:informer-creation-error
   #:informer-creation-error-type

   #:no-transports-error

   #:event-error
   #:event-error-event

   #:event-type-error

   #:event-scope-error
   #:event-error-expected-scope)

  ;; Restarts
  (:export
   #:retry
   ;; continue provided by CL
   ;; use-value likewise
   ;; abort likewise

   #:use-scope
   #:use-uri)

  ;; (Special) Variables
  (:export
   #:*reserved-scope*

   #:*framework-timestamps*

   #:*id-random-state*

   #:*default-configuration*
   #:*default-configuration-files*)

  ;; Reloading
  (:export
   #:enable-id-random-state-reseed)

  ;; Hooks
  (:export
   #:error-hook)

  ;; Error handling
  (:export
   #:call-with-restart-and-timeout
   #:with-restart-and-timeout)

  ;; Meta data protocol
  (:export
   #:meta-data

   #:meta-data-count
   #:meta-data-keys
   #:meta-data-values

   #:meta-data-plist
   #:meta-data-alist)

  ;; Timestamp protocol
  (:export
   #:timestamp

   #:timestamp-count
   #:timestamp-keys
   #:timestamp-values

   #:timestamp-plist
   #:timestamp-alist)

  ;; Component URL protocol
  (:export
   #:relative-url
   #:abstract-uri
   #:transport-specific-urls)

  ;; Scope protocol
  (:export
   #:scope
   #:scope-component
   #:scope-components
   #:scope-string
   #:scope-interned?

   #:scope=       #:scope=/no-coerce
   #:sub-scope?   #:sub-scope?/no-coerce
   #:super-scope? #:super-scope?/no-coerce

   #:make-scope
   #:super-scopes
   #:merge-scopes

   #:intern-scope)

  ;; Event id
  (:export
   #:event-id

   #:event-id=
   #:event-id->uuid)

  ;; Event
  (:export
   #:event
   #:event-id
   #:event-id/opaque
   #:event-scope
   #:event-origin
   #:event-sequence-number
   #:event-method
   #:event-data
   #:event-meta-data
   #:event-timestamps
   #:event-causes

   #:make-event

   #:event=

   #:print-event-data)

  ;; Participant protocol and `participant' class
  (:export
   #:participant
   #:participant-kind
   #:participant-id
   #:participant-scope
   #:participant-converters
   #:participant-converter
   #:participant-transform
   #:participant-error-hook

   #:make-participant
   #:make-participant-using-class

   #:detach #:detach/ignore-errors)

  ;; Protocol for receiving participants
  (:export
   #:receiver-filters)

  ;; Listener
  (:export
   #:listener

   #:make-listener)

  ;; Reader protocol and `reader' class
  (:export
   #:receive

   #:reader

   #:make-reader)

  ;; Send protocol and `informer' class
  (:export
   #:send

   #:informer

   #:make-informer)

  ;; Configuration
  (:export
   #:options-from-environment
   #:options-from-stream
   #:options-from-default-sources

   #:option-value
   #:section-options

   #:make-options
   #:make-options-for-connector-classes
   #:make-options-for-connector-class

   #:&inherit

   #:transport-options

   #:default-converters
   #:default-converter)

  ;; URI-related function
  (:export
   #:uri-options
   #:uri->scope-and-options)

  ;; Macros
  (:export
   #:call-with-participant
   #:with-participant

   #:define-with-participant-macro

   #:with-reader

   #:with-listener
   #:call-with-handler
   #:with-handler

   #:with-informer)

  ;; Utility functions and macros
  (:export
   #:print-id
   #:print-unreadable-id-object

   #:uuid-mixin

   #:scope-mixin

   #:uri-mixin

   #:define-participant-creation-uri-methods
   #:define-participant-creation-restart-method

   #:maybe-shorten-sequence)

  ;; Optimization settings
  (:import-from #:cl-rsb-system
   #:+optimization-fast+unsafe+)

  (:documentation
   "This package contains most of the basic, client-facing
functionality of cl-rsb:
+ `event' class
+ participant classes and associated protocols
  + `reader' participant class
    + `make-reader' method, `with-reader' macro
    + `receiver-filters' accessor
    + `receive' method
  + `listener' participant class
    + `make-listener' method, `with-listener' macro
    + `receiver-filters' accessor
  + `informer' participant class
    + `make-informer' method, `with-informer macro
    + `send' method
+ configuration system"))
