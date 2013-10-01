;;;; receiving-client.lisp --- Superclass for receiving, filtering clients.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb)

(defclass receiving-client (participant
                            rsb.ep:client)
  ((filters :type     list
            :initform nil
            :accessor receiver-filters
            :documentation
            "The list of filters that events have to match in order to
be received by the participant."))
  (:documentation
   "This class is intended to be used as a superclass of event
processing configuration clients that receive and filter events."))

(defmethod (setf receiver-filters) :around ((new-value   list)
                                            (participant receiving-client))
  "Notify interested parties of the change in the set of
listeners."
  (let+ (((&accessors-r/o
           (old-value    receiver-filters)
           (configurator rsb.ep:client-configurator)) participant))
    (prog1
        (call-next-method)
      (let ((added   (set-difference new-value old-value))
            (removed (set-difference old-value new-value)))
        (log1 :info participant "Added   filters ~{~S~^, ~}" added)
        (log1 :info participant "Removed filters ~{~S~^, ~}" removed)

        (iter (for filter in added)
              (rsb.ep:notify configurator filter :filter-added))
        (iter (for filter in removed)
              (rsb.ep:notify configurator filter :filter-removed))))))

(defmethod print-object ((object receiving-client) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A |(~D)"
            (scope-string (participant-scope object))
            (length (receiver-filters object)))))
