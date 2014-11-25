;;;; drop-payload.lisp --- A transform that removes payloads from events.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform)

(defconstant +dropped-payload+ '%dropped-payload
  "This object is used to represent a removed payload.")

(deftype dropped-payload ()
  "Instances of this type represent removed payloads."
  '(eql %payload-dropped))

(defclass drop-payload ()
  ()
  (:documentation
   "Removes payloads from events.

    This transform is intended for use in situations in which large
    numbers of events have to be retained with only the meta-data
    being of interest."))

(service-provider:register-provider/class
 'transform :drop-payload :class 'drop-payload)

(defmethod transform! ((transform drop-payload) (event event))
  (setf (event-data event) +dropped-payload+)
  event)
