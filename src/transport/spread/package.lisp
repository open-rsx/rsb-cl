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
    Spread group communication system."))

(cl:in-package #:rsb.transport.spread)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :rsb.transport.spread.num-fragments *transport-metrics*))
