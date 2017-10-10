;;;; package.lisp --- Package definition for the transport.socket module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.socket
  (:nicknames #:rsb.tp.sock)

  (:shadow
   #:connector)

  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:nibbles

   #:rsb
   #:rsb.event-processing
   #:rsb.transport
   #:rsb.protocol)

  (:shadowing-import-from #:rsb
   #:event-id
   #:event-meta-data)

  ;; Conditions
  (:export
   #:socket-bus-auto-connection-error)

  (:documentation
   "This package contains a transport implementation that uses
    multiple point-to-point socket connections to simulate a bus."))

(cl:in-package #:rsb.transport.socket)

(define-constant +backscatter-warning+
    #.(format
       nil
       "Since currently all events are independently delivered to all ~
        processes, regardless of whether the individual processes ~
        actually contain participants, this transport cannot work ~
        efficiently with participants distributed across many ~
        processes.")
    :test #'string=)
