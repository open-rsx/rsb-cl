;;;; protocol.lisp --- Protocol provided by the model module.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.model)

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

(defgeneric process-info-executing-user (info)
  (:documentation
   "Return the login- or account-name of the user executing the
    process represented by INFO."))

(defgeneric process-info-rsb-version (info)
  (:documentation
   "Return the RSB version used in the process represented by INFO."))

(defgeneric process-info-display-name (info)
  (:documentation
   "Return nil or the supplied display name of the process represented
    by INFO."))

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

(defgeneric host-info-machine-type (info)
  (:documentation
   "Return the type of the machine, usually CPU architecture, of the
    host represented by INFO."))

(defgeneric host-info-machine-version (info)
  (:documentation
   "Return the version of the machine within its type, usually the CPU
    identification string, of the host represented by INFO."))

(defgeneric host-info-software-type (info)
  (:documentation
   "Return the type of the operating system, usually the kernel name,
    running on the host represented by INFO."))

(defgeneric host-info-software-version (info)
  (:documentation
   "Return the version of the operating system within its type,
    usually the kernel version string, of the host represented by
    INFO."))

;; Remote host information protocol

(defgeneric host-info-state (info)
  (:documentation
   "Return a `host-state' value indicating the current (assumed) state
    of the host represented by INFO.

    See `host-state' type."))

;;; Node protocol
;;;
;;; Each node has an associated "*-info instance" (e.g. `host-info',
;;; `remote-host-info', `process-info', etc.) associated to
;;; it. Also, nodes form a hierarchy:
;;;
;;; host node ─> process node ─> participant node ─┐
;;;                                     ⌃          │
;;;                                     └──────────┘

(defgeneric node-info (node)
  (:documentation
   "Return the *-info instance associated to NODE."))

(defgeneric node-parent (node)
  (:documentation
   "Return the node which is the parent of NODE or nil if there is
    none."))

(defgeneric node-children (node)
  (:documentation
   "Return the list of nodes which are the children of NODE."))

;; Default behavior

(defmethod node-parent ((node t))
  nil)

(defmethod node-children ((node t))
  '())

;; Local Variables:
;; coding: utf-8
;; End:
