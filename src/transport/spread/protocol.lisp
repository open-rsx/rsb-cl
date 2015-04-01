;;;; protocol.lisp --- Protocol used by the spread transport implementation.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; Spread connection protocol

(defgeneric disconnect (connection)
  (:documentation
   "Disconnect CONNECTION from the Spread daemon."))

(defgeneric ref-group (connection group)
  (:documentation
   "Increase the reference count of GROUP, causing CONNECTION to join
    the Spread group named GROUP in case of a 0 -> 1 transition of the
    reference count."))

(defgeneric unref-group (connection group)
  (:documentation
   "Decrease the reference count of GROUP, causing CONNECTION to leave
    the Spread group named GROUP in case of a 1 -> 0 transition of the
    reference count.

    Return two values: 1) number of remaining references to GROUP 2)
    number of remaining referenced groups in CONNECTION."))

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
    ASSEMBLY. Warning conditions are signaled if FRAGMENT cannot be
    integrated for some reason. The (possibly) modified ASSEMBLY is
    returned."))

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
   "Merge NOTIFICATION into the appropriate assembly within POOL. If
    NOTIFICATION completes the assembly, return a notification
    instance built from the complete assembly. Otherwise, return
    nil."))
