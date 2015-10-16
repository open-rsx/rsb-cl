;;;; drop-payload.lisp --- A transform that removes payloads from events.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform)

(defconstant +dropped-payload+ '%dropped-payload
  "This object is used to represent a removed payload.")

(deftype dropped-payload ()
  "Instances of this type represent removed payloads."
  '(eql %payload-dropped))

(defclass drop-payload (funcallable-transform-mixin)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Removes payloads from events.

    This transform is intended for use in situations in which large
    numbers of events have to be retained with only the meta-data
    being of interest."))

(service-provider:register-provider/class
 'transform :drop-payload :class 'drop-payload)

(defmethod rsb.ep:access? ((transform drop-payload)
                           (part      (eql :data))
                           (mode      (eql :write)))
  t)

(defmethod transform! ((transform drop-payload) (event event))
  (setf (event-data event) +dropped-payload+)
  event)
