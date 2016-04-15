;;;; package.lisp --- Package definition for transport.spread module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.spread
  (:nicknames #:rsb.tp.spread)

  (:shadowing-import-from #:rsb.protocol
   #:event-id
   #:event-meta-data)

  (:shadow
   #:connector)

  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:nibbles

   #:rsb
   #:rsb.event-processing
   #:rsb.transport
   #:rsb.protocol)

  ;; Conditions
  (:export
   #:assembly-problem
   #:assembly-problem-assembly

   #:fragment-problem
   #:assembly-problem-fragment

   #:invalid-fragment-id

   #:duplicate-fragment

   #:fragmentation-problem

   #:insufficient-room
   #:fragmentation-problem-required
   #:fragmentation-problem-available)

  ;; Variables and constants
  (:export
   #:+protocol-version+)

  ;; Connection protocol
  (:export
   #:connection-name
   #:connection-daemon-name
   #:connection-groups

   #:ref-group
   #:unref-group

   #:receive-message
   #:send-message)

  (:documentation
   "This package contains a transport implementation based on the
spread group communication system."))

(cl:in-package #:rsb.transport.spread)

(defconstant +protocol-version-base+ 100
  "Constant that has to be subtracted from the field number of the

  .rsb.protocol.Notification.event_id

field to obtain the wire protocol version.")

(defconstant +protocol-version+
  (let ((field-number (pb::field-desc-number
                       (pb:find-descriptor
                        ".rsb.protocol.Notification.event_id"))))
    (- field-number +protocol-version-base+))
  "Spread wire protocol version. Determined by subtracting the value
of `+protocol-version-base+' from the field number of the

  .rsb.protocol.Notification.event_id

field.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :rsb.transport.spread.num-fragments *transport-metrics*))
