;;;; client.lisp --- A client that knows its configurator.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defclass client ()
  ((configurator :initarg  :configurator
                 :type     configurator
                 :reader   client-configurator
                 :documentation
                 "Stores the configurator instance working for this
                  client."))
  (:default-initargs
   :configurator (missing-required-initarg 'client :configurator))
  (:documentation
   "A client of the event processing subsystem in the sense that they
    have an associated `configurator'."))

(defmethod transport-specific-urls ((component client))
  ;; Return a list of transport URLs for connectors used by COMPONENT.
  (remove-duplicates
   (mapcar (lambda (connector)
             (uiop:symbol-call '#:rsb.transport '#:connector-relative-url
                               connector component))
           (configurator-connectors (client-configurator component)))
   :test #'puri:uri=))

(defmethod detach ((participant client))
  ;; Let PARTICIPANT's configurator do the heavy lifting.
  (detach (client-configurator participant)))
