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

(defgeneric process-info-executing-user (info)
  (:documentation
   "Return the login- or account-name of the user executing the
    process represented by INFO."))

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

;;; Participant table protocol
;;;
;;; Retrieval and manipulation of `participant-info' instances indexed
;;; by id.

(defgeneric introspection-participants (container)
  (:documentation
   "Return the list of all `participant-info' instances stored in
    CONTAINER."))

(defgeneric (setf introspection-participants) (new-value container)
  (:documentation
   "Store NEW-VALUE as the list of `participant-info' instances in
    CONTAINER."))

(defgeneric introspection-participants/roots (container)
  (:documentation
   "Return the list of root (i.e. without parent) `participant-info'
    instances stored in CONTAINER."))

(defgeneric find-participant (id container
                              &key
                              parent-id
                              if-does-not-exist)
  (:documentation
   "Return the `participant-info' instance designated by ID in
    CONTAINER.

    PARENT-ID is accepted for parity with (setf find-participant).

    IF-DOES-NOT-EXIST controls the behavior in case ID does not
    designate a participant entry in CONTAINER."))

(defgeneric (setf find-participant) (new-value id container
                                     &key
                                     parent-id
                                     if-does-not-exist)
  (:documentation
   "Store NEW-VALUE as the `participant-info' instance designated by
    ID in CONTAINER.

    If NEW-VALUE is nil, remove the corresponding entry.

    IF-DOES-NOT-EXIST is accepted for parity with
    `find-participant'."))

(defgeneric ensure-participant (id container default)
  (:documentation
   "If the participant designated by ID exists in CONTAINER,
    return it. Otherwise create it according to DEFAULT, store and
    return it.

    DEFAULT is of the form

      (CLASS . INITARGS)

    and maybe be passed to `make-instance' or `change-class' depending
    on the existing entry for ID."))

;; Default behavior

(defmethod introspection-participants/roots ((container t))
  (remove-if #'entry-parent (introspection-participants container)))

;;; Process table protocol

(defgeneric introspection-processes (container)
  (:documentation
   "Return the list of `process-info' instances stored in
    CONTAINER."))

(defgeneric find-process (id container)
  (:documentation
   "Return the process designated by ID in CONTAINER."))

(defgeneric (setf find-process) (new-value id container)
  (:documentation
   "Store NEW-VALUE as the process designated by ID in CONTAINER."))

(defgeneric ensure-process (id container process &key receiver)
  (:documentation
   "If the process designated by ID exists in CONTAINER, update it
    with information from PROCESS and return it. Otherwise create and
    return a new entry based on PROCESS.

    RECEIVER is the introspection receiver through which PROCESS was
    received. This relation is recorded in the updated or created
    entry."))

;;; Host table protocol

(defgeneric introspection-hosts (container)
  (:documentation
   "Return the list of `host-info' instances stored in CONTAINER."))

(defgeneric find-host (id container)
  (:documentation
   "Return the host designated by ID in CONTAINER."))

(defgeneric (setf find-host) (new-value id container)
  (:documentation
   "Store NEW-VALUE as the host designated by ID in CONTAINER."))

(defgeneric ensure-host (id container host)
  (:documentation
   "If the host designated by ID exists in CONTAINER, return
    it. Otherwise create and return a new entry based on HOST."))

;;; Database locking protocol

(defgeneric call-with-database-lock (database thunk)
  (:documentation
   "Call THUNK with locked DATABASE."))

;;; Timing tracker protocol

(defgeneric timing-tracker-clock-offset (tracker)
  (:documentation
   "Return the clock offset estimated by TRACKER or nil if not
    available."))

(defgeneric timing-tracker-latency (tracker)
  (:documentation
   "Return the communication latency estimated by TRACKER or nil if
    not available."))

(defgeneric timing-tracker-to-local-clock (tracker time &key allow-future?)
  (:documentation
   "Return a timestamp corresponding to TIME but expressed with
    respect to the local clock.

    If possible, the clock offset estimate computed by TRACKER is used
    to perform the conversion.

    ALLOW-FUTURE? controls whether returned timestamps are restricted
    to the current time of the local clock or earlier."))

;;; Database change hook protocol

(defgeneric database-change-hook (database)
  (:documentation
   "Return a hook that is run when DATABASE changes."))

;;; Introspection receiver protocol

(defgeneric introspection-survey (introspection)
  (:documentation
   "Perform an introspection survey collecting the results in
    INTROSPECTION."))

(defgeneric introspection-ping (introspection host process
                                &key
                                block?)
  (:documentation
   "Send a \"ping\" message to PROCESS running on HOST and return the
    reply event.

    BLOCK? controls whether the call should block until the reply
    arrives. If false, a `rsb.patterns.request-reply:future' is
    returned."))

;;; Remote introspection database protocol
;;;
;;; Each entry has an associated "*-info instance" (e.g. `host-info',
;;; `remote-host-info', `process-info', etc.) associated to
;;; it. Also, entries form a hierarchy:
;;;
;;; `host-entry' ─> `process-entry' ─> `participant-entry' ─┐
;;;                                             ⌃           │
;;;                                             └───────────┘

(defgeneric entry-info (entry)
  (:documentation
   "Return the *-info instance associated to ENTRY."))

(defgeneric entry-parent (entry)
  (:documentation
   "Return the entry which is the parent of ENTRY or nil if there is
    none."))

(defgeneric entry-children (entry)
  (:documentation
   "Return the list of entries which are the children of ENTRY."))

;; Local Variables:
;; coding: utf-8
;; End:
