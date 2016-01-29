;;;; package.lisp --- Package definition for the transport module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport
  (:nicknames #:rsb.tp)

  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:nibbles

   #:rsb
   #:rsb.event-processing
   #:rsb.converter)

  ;; Conditions
  (:export
   #:connector-construction-failed
   #:connector-construction-failed-name
   #:connector-construction-failed-direction
   #:connector-construction-failed-args

   #:connection-closed
   #:connection-closed-connection

   #:connection-unexpectedly-closed

   #:no-suitable-converter
   #:connector-construction-failed-wire-type
   #:connector-construction-failed-candidates

   #:decoding-error
   #:decoding-error-encoded

   #:encoding-error
   #:encoding-error-event)

  ;; Variables
  (:export
   #:*transport-metrics*)

  ;; Transport protocol
  (:export
   #:transport-wire-type)           ; works on transport names,
                                    ; transport instances, connector
                                    ; classes and connector instances

  ;; Connector protocol
  (:export
   #:connector-transport

   #:connector-direction            ; work on connector classes and instances

   #:connector-url                  ; work on connector instances
   #:connector-relative-url

   #:connector-schemas              ; work on connector classes, not instances
   #:connector-options)

  ;; Transport class family and connector creation
  (:export
   #:no-such-transport
   #:find-transport-class
   #:transport-classes
   #:find-connector-class)

  ;; Transport and connector registration protocol
  (:export
   #:register-transport
   #:register-connector)

  ;; `transport' class and service
  (:export
   #:transport)

  ;; Connector creation
  (:export
   #:make-connector
   #:make-connectors)

  ;; `connector-class' metaclass
  (:export
   #:connector-class

   #:&slot)

  ;; `connector' class
  (:export
   #:connector)

  ;; `conversion-mixin' class
  (:export
   #:conversion-mixin
   #:connector-converter)

  ;; Notification receiver protocol
  (:export
   #:receive-notification
   #:notification->event)

  ;; Notification sender protocol
  (:export
   #:send-notification
   #:event->notification)

  ;; Threaded receiver protocol and `threaded-receiver-mixin' class
  (:export
   #:start-receiver
   #:stop-receiver
   #:exit-receiver

   #:receive-messages

   #:threaded-receiver-mixin
   #:connector-started?
   #:connector-thread)

  ;; `timestamping-receiver-mixin' class
  (:export
   #:timestamping-receiver-mixin)

  ;; `timestamping-sender-mixin' class
  (:export
   #:timestamping-sender-mixin)

  ;; Error handling mixin classes
  (:export
   #:error-handling-push-receiver-mixin

   #:error-handling-pull-receiver-mixin

   #:error-handling-sender-mixin)

  ;; `restart-notification-receiver-mixin' class
  (:export
   #:restart-notification-receiver-mixin)

  ;; `restart-notification-sender-mixin' class
  (:export
   #:restart-notification-sender-mixin)

  ;; `threaded-message-receiver-mixin' class
  (:export
   #:threaded-message-receiver-mixin)

  ;; `expose-transport-metrics-mixin' class
  (:export
   #:expose-transport-metrics-mixin
   #:connector-expose
   #:connector-expose?)

  (:documentation
   "This package contains the transport layer.

    The central concepts of the transport layer are \"transport\" and
    \"connector\".  Connector instances handle incoming and outgoing
    events (see `rsb.event-processing:handle'). The function
    `make-connector' can be used to create connector instances for
    different transports.

    The efficiency of data handling can be increased by notifying
    connectors of restrictions that can be applied to the otherwise
    broadcast-style event delivery."))
