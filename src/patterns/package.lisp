;;;; package.lisp --- Package definition for the patterns module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rsb)

  ;; Conditions
  (:export
   #:child-condition
   #:child-condition-container
   #:child-condition-key

   #:no-such-child-error

   #:child-exists-error
   #:child-exists-error-child

   #:protocol-condition
   #:protocol-condition-protocol
   #:protocol-condition-role
   #:protocol-condition-message

   #:simple-protocol-condition

   #:protocol-warning

   #:protocol-error)

  ;; Participant children protocol
  (:export
   #:participant-children)

  ;; Child participant lookup protocol
  (:export
   #:participant-child)                 ; also `setf'

  ;; Participant child creation protocol
  (:export
   #:make-child-scope
   #:make-child-initargs
   #:make-child-participant)

  ;; `composite-participant-mixin' mixin class
  (:export
   #:composite-participant-mixin)

  ;; `child-container-mixin' mixin class
  (:export
   #:child-container-mixin)

  ;; `configuration-inheritance-mixin' mixin class
  (:export
   #:configuration-inheritance-mixin)

  ;; `lazy-child-making-mixin' mixin class
  (:export
   #:lazy-child-making-mixin)

  (:documentation
   "This package contains infrastructure for implementing
    communication patterns on top of the event-driven architecture at
    the core of RSB.

    Currently the following infrastructure is provided:

    * Participant children protocol

    * Child participant lookup protocol

    * Child participant creation protocol

    Sub-packages contain concrete communication patterns implemented
    on top of the RSB core, potentially utilizing the infrastructure
    provided by this package.

    Currently the following patterns are implemented in sub-packages:

    request-reply (package rsb.patterns.request-reply)

      In this communication pattern, a client submits requests to a
      server which processes the requests and sends associated replies
      to the client."))
