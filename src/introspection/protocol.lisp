;;;; protocol.lisp --- Protocol provided by the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Most recent activity protocol

(defgeneric info-most-recent-activity (info)
  (:documentation
   "Return the time of the most recent activity on INFO as a
    `local-time:timestamp'."))

;;; Timing protocol

(defgeneric info-clock-offset (info)
  (:documentation
   "Return the estimated offset in seconds of the clock used by the
    remote object INFO relative to the local clock or nil if
    unknown."))

(defgeneric info-latency (info)
  (:documentation
   "Return the estimated latency in seconds for communicating with the
    remote object INFO."))

;;; Participant information protocol

(defgeneric participant-info-kind (info)
  (:documentation
   "Return the kind of the participant represented by INFO."))

(defgeneric participant-info-id (info)
  (:documentation
   "Return the unique id of the participant represented by INFO."))

(defgeneric participant-info-parent-id (info)
  (:documentation
   "Return nil or the id of the participant that is the parent of the
    participant represented by INFO."))

(defgeneric participant-info-scope (info)
  (:documentation
   "Return the scope of the participant represented by INFO."))

(defgeneric participant-info-type (info)
  (:documentation
   "Return the type of the participant represented by INFO.

    Note: subject to change."))

(defgeneric participant-info-transports (info)
  (:documentation
   "Return a list of `puri:uri' instances describing the transports
    through which the participant represented by INFO is connected to
    the bus."))

;;; Process information protocol

(defgeneric process-info-process-id (info)
  (:documentation
   "Return the numeric operating system id of the process represented
    by INFO."))

(defgeneric process-info-program-name (info)
  (:documentation
   "Return the name program of the program executing in the process
    represented by INFO.

    The returned value is a string which can designate an absolute
    path but also other things such as an interpreter and the script
    it executes."))

(defgeneric process-info-commandline-arguments (info)
  (:documentation
   "Return a list of commandline arguments with which the process
    represented by INFO has been started.

    Unlike POSIX behavior, the first element of returned list is not
    the name of the program name."))

(defgeneric process-info-start-time (info)
  (:documentation
   "Return a `local-time:timestamp' instance corresponding to the
    start time of the process represented by INFO."))

;; Remote process information protocol

(defgeneric process-info-state (info)
  (:documentation
   "Return a `process-state' value indicating the current (assumed)
    state of the process represented by INFO.

    See `process-state' type."))

(defgeneric process-info-transports (info)
  (:documentation
   "Return a list of `puri:uri' instances describing the transports
    through which the process represented by INFO has been
    contacted."))

;;; Host information protocol

(defgeneric host-info-id (info)
  (:documentation
   "Return nil or a string uniquely identifying the host represented
    by INFO.

    If nil, `host-info-hostname' should be used as a fallback unique
    id."))

(defgeneric host-info-hostname (info)
  (:documentation
   "Return the name of the host represented by INFO."))

;; Remote host information protocol

(defgeneric host-info-state (info)
  (:documentation
   "Return a `host-state' value indicating the current (assumed) state
    of the host represented by INFO.

    See `host-state' type."))

;;; Hello and bye messages
;;;
;;; Hello messages are sent when participants are created and when an
;;; introspection survey is performed. These messages contain
;;; information about the respective participants but also the
;;; containing processes and hosts.
;;;
;;; Bye messages only contain unique ids as their purpose is removing
;;; previously announced participants from introspection databases.

(defgeneric hello-participant (message)
  (:documentation
   "Return a `participant-info' instance representing the participant
    to which MESSAGE refers."))

(defgeneric hello-process (message)
  (:documentation
   "Return a `process-info' instance representing the process
    containing the participant to which MESSAGE refers."))

(defgeneric hello-host (message)
  (:documentation
   "Return a `host-info' instance representing the host on which the
    process containing the participant to which MESSAGE refers is
    executed."))

(defgeneric bye-id (message)
  (:documentation
   "Return the unique id of the participant to which MESSAGE
    refers."))
