;;;; timestamping-receiver-mixin.lisp --- A protocol for receiving and decoding messages.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport)


;;; Mixin class `timestamping-receiver-mixin'
;;

(defclass timestamping-receiver-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into connector classes
that perform two tasks:
+ receive messages
+ decode received messages
The associated protocol is designed to be
direction-agnostic (i.e. should work for both push and pull)."))

(defmethod message->event :around ((connector   timestamping-receiver-mixin)
				   (message     t)
				   (wire-schema t))
  "Add a :receive timestamp to the generated event, if any."
  (let ((event (call-next-method)))
    (when event
      (setf (timestamp event :receive) (local-time:now)))
    event))
