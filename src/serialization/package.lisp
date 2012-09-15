;;;; package.lisp --- Package definition for serialization module.
;;;;
;;;; Copyright (C) 2012, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.serialization
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:nibbles

   #:rsb
   #:rsb.converter)

  (:export
   #:notification->event
   #:event->notification)

  ;; Utility functions
  (:export
   #:unix/microseconds->timestamp
   #:timestamp->unix/microseconds

   #:bytes->wire-schema
   #:wire-schema->bytes)

  ;; Optimization settings
  (:import-from #:cl-rsb-system
   #:+optimization-fast+unsafe+)

  (:documentation
   "This package contains functions and classes which implement
    the (de)serialization of events to/from different kinds of
    notifications.

    The primary interface for this event (de)serialization are the
    generic functions

    notification->event                 [generic function]

      Deserialize a notification into an event.

    event->notification                 [generic function]

      Serialize an event into a notification.

    The package also contains utility functions for:

    * conversion of timestamps
    * (de)serialization of wire-schemas"))
