;;;; receiving-client.lisp --- Superclass for receiving, filtering clients.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass receiving-client (participant
                            direction-mixin
                            rsb.ep:client)
  ((filters :type     list
            :accessor receiver-filters
            :initform '()
            :documentation
            "The list of filters that events have to match in order to
             be received by the participant."))
  (:documentation
   "This class is intended to be used as a superclass of event
    processing configuration clients that receive and filter
    events."))

(defmethod shared-initialize :after ((instance   receiving-client)
                                     (slot-names t)
                                     &key
                                     (filters '() filters-supplied?))
  (when filters-supplied?
    (setf (receiver-filters instance) filters)))

(defmethod (setf receiver-filters) :around ((new-value   list)
                                            (participant receiving-client))
  ;; Notify interested parties of the change in the set of listeners.
  (let* ((configurator (rsb.ep:client-configurator participant))
         (old-value    (receiver-filters participant))
         (added        (set-difference new-value old-value))
         (removed      (set-difference old-value new-value)))
    (prog1
        (call-next-method)
      (log:debug "~@<~A~@:_~
                    ~2@Tadded   filters ~:A~@:_~
                    ~2@Tremoved filters ~:A~
                  ~@:>"
                 participant added removed)
      (iter (for filter in added)
            (rsb.ep:notify configurator filter :filter-added))
      (iter (for filter in removed)
            (rsb.ep:notify configurator filter :filter-removed)))))

(defmethod print-object ((object receiving-client) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A |(~D)"
            (scope-string (participant-scope object))
            (length (receiver-filters object)))))
