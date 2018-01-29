;;;; package.lisp --- Package definition for the model module.
;;;;
;;;; Copyright (C) 2015, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.model
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  (:import-from #:rsb
   #:scope
   #:scope-string)

  ;; "Info" model protocol and classes
  (:export
   #:info-most-recent-activity
   #:info-most-recent-activity-difference

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
   #:process-info-executing-user
   #:process-info-rsb-version
   #:process-info-display-name

   #:host-info                          #:remote-host-info
   #:host-info-id                       #:host-info-state
   #:host-info-hostname
   #:host-info-machine-type
   #:host-info-machine-version
   #:host-info-software-type
   #:host-info-software-version)

  ;; Info update protocol
  (:export
   #:update-using-info)

  ;; "Node" model protocol and classes
  (:export
   #:node-info

   #:node-parent
   #:node-children

   #:info-mixin
   #:parented-mixin
   #:child-container-mixin

   #:node
   #:participant-node
   #:process-node
   #:host-node)

  (:documentation
   "This package contains protocols and classes for modeling RSB
    systems. The following model classes are provided:

    * \"info\" classes storing properties of different kinds of
      entities in a system:

      * `participant-info' and `remote-participant-info' which
        describe participants in general and participants in remote
        processes respectively.

      * `process-info' and `remote-process-info' which describe
        processes in general and processes on remote hosts
        respectively.

      * `host-info' and `remote-host-info' which describe hosts in
        general and in remote hosts respectively.

    * \"node\" classes representing the relations between entities in
      a system:

      * `node' is the superclass of all \"relation\" classes

      * Instances of `participant-node', `process-node' and
        `host-node' each contain an associated `participant-info',
        `process-info' or `host-info' respectively and represent
        relations to other nodes."))
