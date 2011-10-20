;;; package.lisp ---
;;
;; Copyright (C) 2010, 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :cl-user)

(defpackage :rsb
  (:use
   :cl
   :alexandria
   :split-sequence
   :iterate
   :bind
   :hooks)

  ;; Types
  (:export
   :octet-vector

   :sequence-number

   :error-policy

   :implementation-feedback
   :direction
   :wire-type)

  ;; Conditions
  (:export
   :chainable-condition
   :chainable-condition-cause
   :maybe-print-cause

   :maybe-print-explanation

   :missing-required-argument

   :missing-required-initarg

   :rsb-error

   :communication-error

   :participation-failed
   :participation-failed-scope
   :participation-failed-transports

   :listener-creation-failed

   :reader-creation-failed

   :informer-creation-failed
   :informer-creation-failed-type

   :no-transports

   :invalid-event
   :invalid-event-event

   :invalid-event-type

   :invalid-event-scope
   :invalid-event-expected-scope)

  ;; Restarts
  (:export
   :retry

   ;; continue provided by CL
   ;; use-value likewise

   ;; log is in CL
   :log-error

   ;; warn is in CL
   :signal-warning

   :use-scope
   :use-uri)

  ;; Special Variables
  (:export
   :*default-configuration*)

  ;; Hooks
  (:export
   :error-hook)

  ;; Meta data protocol
  (:export
   :meta-data

   :meta-data-count
   :meta-data-keys
   :meta-data-values

   :meta-data-plist
   :meta-data-alist)

  ;; Timestamp protocol
  (:export
   :timestamp

   :timestamp-count
   :timestamp-keys
   :timestamp-values

   :timestamp-plist
   :timestamp-alist)

  ;; Component URL protocol
  (:export
   :relative-url
   :abstract-uri
   :transport-specific-urls)

  ;; Scope
  (:export
   :scope
   :scope-component
   :scope-components
   :scope-string
   :scope-interned?

   :scope=
   :sub-scope?
   :super-scope?

   :make-scope
   :super-scopes
   :merge-scopes

   :intern-scope)

  ;; Event
  (:export
   :event
   :event-sequence-number
   :event-id
   :event-scope
   :event-origin
   :event-method
   :event-type
   :event-data
   :event-meta-data
   :event-timestamps
   :event-causes

   :make-event
   :make-event/typed

   :event=

   :event-id->uuid

   :print-event-data)

  ;; Participant protocol and `participant' class
  (:export
   :detach :detach/ignore-errors

   :participant
   :participant-id
   :participant-scope
   :participant-converters
   :participant-converter)

  ;; Protocol for receiving participants
  (:export
   :receiver-filters)

  ;; Listener
  (:export
   :listener

   :make-listener)

  ;; Reader protocol and `reader' class
  (:export
   :receive

   :reader

   :make-reader)

  ;; Send protocol and `informer' class
  (:export
   :send

   :informer

   :make-informer)

  ;; Configuration
  (:export
   :options-from-environment
   :options-from-stream
   :options-from-default-sources

   :option-value
   :section-options

   :make-options
   :make-options-for-connector-classes
   :make-options-for-connector-class

   :&inherit

   :transport-options
   :default-converters)

  ;; URI-related function
  (:export
   :uri-options
   :uri->scope-and-options)

  ;; Macros
  (:export
   :with-reader

   :with-listener
   :with-handler

   :with-informer)

  ;; Logging
  (:export
   :log1)

  ;; Utility functions and macros
  (:export
   :print-id
   :print-unreadable-id-object

   :uuid-mixin

   :scope-mixin

   :uri-mixin

   :make-participant
   :define-participant-creation-uri-methods
   :define-participant-creation-restart-method

   :maybe-shorten-sequence

   :hostname)

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
