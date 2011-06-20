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
   :make-connector)

  ;; `connector-class' metaclass
  (:export
   :connector-class)

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

(in-package :rsb.transport)

(log5:defcategory rsb.transport)

(defmacro log1 (category format &rest args)
  `(log5:log-for (or :rsb.transport ,(intern (string category) :log5))
		 ,format ,@args))
