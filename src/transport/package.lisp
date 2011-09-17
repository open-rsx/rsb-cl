;;; package.lisp --- Package definition for the transport module.
;;
;; Copyright (C) 2011 Jan Moringen
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

(cl:in-package :cl-user)

(defpackage :rsb.transport
  (:nicknames :rsb.tp)

  (:use
   :cl
   :alexandria
   :bind
   :iterate

   :rsb
   :rsb.event-processing
   :rsb.converter)

  ;; Conditions
  (:export
   :connector-construction-failed
   :connector-construction-failed-name
   :connector-construction-failed-direction
   :connector-construction-failed-args

   :no-suitable-converter
   :connector-construction-failed-wire-type
   :connector-construction-failed-candidates

   :decoding-error
   :decoding-error-encoded

   :encoding-error
   :encoding-error-event)

  ;; Connector protocol
  (:export
   :connector-direction ;; work on connector classes and instances
   :connector-wire-type

   :connector-url ; work on connector instances
   :connector-relative-url

   :connector-schemas ;; work on connector classes, not instances
   :connector-options)

  ;; Transport class family and connector creation
  (:export
   :no-such-transport
   :find-transport-class
   :transport-classes

   :find-connector-class
   :make-connector
   :make-connectors)

  ;; `connector-class' metaclass
  (:export
   :connector-class

   &slot)

  ;; `connector' class
  (:export
   :connector)

  ;; `conversion-mixin' class
  (:export
   :conversion-mixin
   :connector-converter)

  ;; Message receiver protocol and `message-receiver-mixin' class
  (:export
   :receive-message
   :message->event

   :message-receiver-mixin)

  ;; Threaded receiver protocol and `threaded-receiver-mixin' class
  (:export
   :start-receiver
   :stop-receiver

   :receive-messages

   :threaded-receiver-mixin
   :connector-started?
   :connector-thread)

  ;; Notification sender protocol
  (:export
   :send-notification
   :event->notification)

  ;; Error handling mixin classes
  (:export
   :error-handling-push-receiver-mixin

   :error-handling-pull-receiver-mixin

   :error-handling-sender-mixin)

  ;; `restart-message-receiver-mixin' class
  (:export
   :restart-message-receiver-mixin)

  ;; `restart-notification-sender-mixin' class
  (:export
   :restart-notification-sender-mixin)

  ;; `threaded-message-receiver-mixin' class
  (:export
   :threaded-message-receiver-mixin)

  ;; `sometimes-interruptible-mixin' class
  (:export
   :sometimes-interruptible-mixin
   :connector-interruptible?)

  ;; `expose-wire-schema-mixin' class
  (:export
   :expose-wire-schema-mixin
   :connector-expose-wire-schema?)

  (:documentation
   "This package contains the transport layer of the RSB Common Lisp
implementation.

The central concept of the transport layer is a \"connector\".
Connector instances handle incoming and outgoing events (see
`rsb.event-processing:handle'). The function `make-connector' can be
used to create connector instances for different kinds of
transports.

The efficiency of data handling can be increased by notifying
connectors of restrictions that can be applied to the otherwise
broadcast-style event delivery."))
