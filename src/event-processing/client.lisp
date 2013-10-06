;;;; configurator-client.lisp --- A client that knows its configurator.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defclass client ()
  ((configurator :initarg  :configurator
                 :type     configurator
                 :reader   client-configurator
                 :documentation
                 "The configurator instance working for this
client."))
  (:default-initargs
   :configurator (missing-required-initarg 'client :configurator))
  (:documentation
   "Instance of this of this class are clients of the event processing
subsystem in the sense that they have an associated `configurator'."))

(defmethod transport-specific-urls ((component client))
  "Return a list of transport URLs for connectors used by COMPONENT."
  (iter (for connector in (configurator-connectors
                           (client-configurator component)))
        (collect
            (funcall (find-symbol "CONNECTOR-RELATIVE-URL" :rsb.transport)
                     connector component))))
