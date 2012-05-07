;;; package.lisp ---
;;
;; Copyright (C) 2010, 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:defpackage :rsb
  (:use
   :cl
   :alexandria
   :split-sequence
   :iterate
   :let-plus
   :hooks
   :more-conditions)

  ;; Types
  (:export
   :scope-designator
   
   :sequence-number

   :error-policy

   :implementation-feedback
   :direction
   :wire-type)

  ;; Conditions
  (:export
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

  ;; (Special) Variables
  (:export
   :*framework-timestamps*

   :*default-configuration*
   :*default-configuration-files*)

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

  ;; Event id
  (:export
   :event-id

   :event-id=
   :event-id->uuid)

  ;; Event
  (:export
   :event
   :event-id
   :event-id/opaque
   :event-scope
   :event-origin
   :event-sequence-number
   :event-method
   :event-data
   :event-meta-data
   :event-timestamps
   :event-causes

   :make-event

   :event=

   :print-event-data)

  ;; Participant protocol and `participant' class
  (:export
   :detach :detach/ignore-errors

   :participant
   :participant-id
   :participant-scope
   :participant-converters
   :participant-converter
   :participant-error-hook)

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

   :default-converters
   :default-converter)

  ;; URI-related function
  (:export
   :uri-options
   :uri->scope-and-options)

  ;; Macros
  (:export
   :define-with-participant-macro

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

   :maybe-shorten-sequence)

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
