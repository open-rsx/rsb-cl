;;; package.lisp --- Package definition for the transport module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

(cl:defpackage :rsb.transport
  (:nicknames :rsb.tp)

  (:use
   :cl
   :alexandria
   :let-plus
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

  ;; Message receiver protocol
  (:export
   :receive-message
   :message->event)

  ;; Notification sender protocol
  (:export
   :send-notification
   :event->notification)

  ;; Threaded receiver protocol and `threaded-receiver-mixin' class
  (:export
   :start-receiver
   :stop-receiver
   :exit-receiver

   :receive-messages

   :threaded-receiver-mixin
   :connector-started?
   :connector-thread)

  ;; `timestamping-receiver-mixin' class
  (:export
   :timestamping-receiver-mixin)

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
