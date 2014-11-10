;;;; package.lisp --- Package definition for the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.introspection
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rsb)

  ;; Conditions
  (:export
   #:introspection-protocol-error

   #:no-such-participant-error
   #:no-such-participant-error-container
   #:no-such-participant-error-id)

  ;; Variables
  (:export
                                      #:+introspection-scope+

   #:introspection-participants-scope #:+introspection-participants-scope+
   #:participant-id->scope
   #:scope->participant-id-or-nil
   #:scope->participant-id

   #:introspection-hosts-scope        #:+introspection-hosts-scope+

   #:introspection-process-scope)

  ;; Model protocol and classes
  (:export
   #:info-most-recent-activity

   #:info-clock-offset
   #:info-latency

   #:participant-info                   #:remote-participant-info
   #:participant-info-kind
   #:participant-info-id
   #:participant-info-parent-id
   #:participant-info-scope
   #:participant-info-type
   #:participant-info-transports

   #:process-info                       #:remote-process-info
   #:process-info-process-id            #:process-info-state
   #:process-info-program-name          #:process-info-transports
   #:process-info-commandline-arguments
   #:process-info-start-time
   #:current-process-info

   #:host-info                          #:remote-host-info
   #:host-info-id                       #:host-info-state
   #:host-info-hostname
   #:current-host-info)

  ;; Introspection message protocol
  (:export
   #:hello
   #:hello-participant
   #:hello-process
   #:hello-host

   #:bye
   #:bye-id)

  ;; Participant table protocol
  (:export
   #:introspection-participants
   #:introspection-participants/roots
   #:find-participant)              ; also `setf'

  ;; Database locking protocol
  (:export
   #:call-with-database-lock
   #:with-database-lock)

  (:documentation
   "This package contains the introspection functionality.

    There are two aspects:

    1) Broadcasting pieces of introspection information describing the
       local host, the current process and participants in it.

    2) Receiving this information and aggregating the individual
       pieces into a global view of the system.

    1) should happen in almost all RSB applications while 2) is mostly
    relevant for tools which inspect and describe system.

    Both aspects share a model protocol and model classes to
    handle (pieces of) introspection information. This model consists
    of the classes:

    * `participant-info' and `remote-participant-info' which describe
      participants in general and participants in remote processes
      respectively.

    * `process-info' and `remote-process-info' which describe
      processes in general and processes on remote hosts respectively.

    * `host-info' and `remote-host-info' which describe hosts in
      general and in remote hosts respectively."))
