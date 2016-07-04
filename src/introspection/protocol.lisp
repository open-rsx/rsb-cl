;;;; protocol.lisp --- Protocol provided by the introspection module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

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
  (remove container (introspection-participants container) ; TODO this is super slow
          :test-not #'eq :key #'node-parent))

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
