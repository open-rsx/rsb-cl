;;;; protocol.lisp --- Protocol used by the spread transport implementation.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; Spread connection protocol

(defgeneric connection-name (connection)
  (:documentation
   "Return local name of CONNECTION."))

(defgeneric connection-daemon-name (connection)
  (:documentation
   "Return name of the daemon to which CONNECTION is connected."))

(defgeneric connection-groups (connection)
  (:documentation
   "Return list of names of Spread groups in which CONNECTION is a
    member."))

(defgeneric ref-group (connection group &key waitable?)
  (:documentation
   "Increase the reference count of GROUP, causing CONNECTION to join
    the Spread group named GROUP in case of a 0 -> 1 transition of the
    reference count.

    Return three values: 1) new number of references to GROUP 2) new
    number of referenced groups in CONNECTION 3) optionally a
    promise (see below).

    If WAITABLE? is true and actual joining occurs (as described
    above), return as the third value an `lparallel:promise' instance
    that can be forced to wait for the completion of the join
    operation.

    Note that `receive-message' has to be called (in blocking or
    non-blocking mode) before or while forcing the promise since
    otherwise Spread events for CONNECTION are not processed."))

(defgeneric unref-group (connection group &key waitable?)
  (:documentation
   "Decrease the reference count of GROUP, causing CONNECTION to leave
    the Spread group named GROUP in case of a 1 -> 0 transition of the
    reference count.

    Return two values: 1) number of remaining references to GROUP 2)
    number of remaining referenced groups in CONNECTION. 3) optionally
    a promise (see below).

    If WAITABLE? is true and actual leaving occurs (as described
    above), return as the third value an `lparallel:promise' instance
    that can be forced to wait for the completion of the leave
    operation.

    Note that `receive-message' has to be called (in blocking or
    non-blocking mode) before or while forcing the promise since
    otherwise Spread events for CONNECTION are not processed."))

(defgeneric receive-message (connection block?)
  (:documentation
   "Receive and return a message from CONNECTION.

    BLOCK? controls whether the call should block until a message has
    been received."))

(defgeneric send-message (connection destination payload)
  (:documentation
   "Send the PAYLOAD to DESTINATION via CONNECTION in a Spread
    message."))

;;; Assembly protocol

(defgeneric add-fragment! (assembly fragment)
  (:documentation
   "Integrate the notification FRAGMENT into the partial assembly
    ASSEMBLY.

    Warning conditions are signaled if FRAGMENT cannot be integrated
    for some reason. The (possibly) modified ASSEMBLY is returned."))

;;; Partial assembly storage protocol

(defgeneric assembly-pool-count (pool)
  (:documentation
   "Return the number of `assembly' instances in POOL."))

(defgeneric ensure-assembly (pool id size)
  (:documentation
   "Find or create an assembly with SIZE total fragments for the event
    identified by ID."))

(defgeneric merge-fragment (pool notification)
  (:documentation
   "Merge NOTIFICATION into the appropriate assembly within POOL.

    If NOTIFICATION completes the assembly, return a notification
    instance built from the complete assembly. Otherwise, return
    nil."))

;;; Transport protocol

(defgeneric ensure-access (transport options connector)
  (:documentation
   "Ensure access for CONNECTOR to the bus described by OPTIONS using TRANSPORT.

    Return an existing or newly created bus object the configuration
    of which matches OPTIONS. In any case, CONNECTOR is already
    attached to the returned bus.

    CONNECTOR may share a Spread connection with other connectors. A
    new Spread connection is only established if a connection
    compatible with OPTIONS is not yet present in TRANSPORT."))
