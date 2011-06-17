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

   :implementation-feedback
   :direction)

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

   :invalid-event
   :invalid-event-event

   :invalid-event-type

   :invalid-event-scope
   :invalid-event-expected-scope)

  ;; Restarts
  (:export

   :log
   :log-error

   :ignore
   :ignore-error

   :use-scope
   :use-uri)

  ;; Special Variables
  (:export
   :*default-configuration*

   :*subscriber-stream*
   :*publisher-stream*)

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
   :relative-url)

  ;; Scope
  (:export
   :scope
   :scope-components
   :scope-string
   :scope-relative? :scope-absolute?

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
   :event-id
   :event-scope
   :event-origin
   :event-type
   :event-data
   :event-meta-data
   :event-timestamps

   :make-event
   :make-event/typed

   :event=)

  ;; Participant
  (:export
   :participant
   :participant-id
   :participant-scope)

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
   :make-options-for-connector-class)

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

  ;; Utility functions and macros
  (:export
   :print-id
   :print-unreadable-id-object)

  ;; TODO temp utils
  (:export
   :uuid-mixin
   :scope-mixin
   :uri-mixin
   :push-source-mixin
   :hostname)
  (:documentation
   "This package provides basic RSB functionality:
+ subscribing to publishers
+ publishing events
+ event processing"))

(in-package :rsb)


;;; Logging
;;

(log5:defcategory rsb)
