;;;; deliver-timestamp-mixin.lisp --- A mixin that adds :deliver timestamps.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.event-processing)

(defclass deliver-timestamp-mixin ()
  ()
  (:documentation
   "This class can be mixed into event processor classes that used in
event delivery contexts to attach :deliver timestamps to processed
events."))

(defmethod handle :before ((processor deliver-timestamp-mixin)
                           (event     event))
  "Attach a :deliver timestamp to EVENT."
  (setf (timestamp event :deliver) (local-time:now)))
