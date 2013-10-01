;;;; expose-transport-metrics-mixin.lisp --- Mixin for connectors that expose transport metrics.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

(defclass expose-transport-metrics-mixin ()
  ((expose :initarg  :expose
           :accessor connector-expose
           :initform nil
           :documentation
           "Controls which metrics of received notifications the
connector should expose in events constructed from these
notifications."))
  (:metaclass connector-class)
  (:options
   (:expose &slot))
  (:documentation
   "This class is intended to be mixed into connector classes that
should be able to store transport metrics of received notifications in
the events constructed from the notifications."))

(defmethod shared-initialize :after ((instance   expose-transport-metrics-mixin)
                                     (slot-names t)
                                     &key
                                     (expose nil expose-supplied?))
  (when expose-supplied?
    (setf (connector-expose instance) expose)))

(defmethod connector-expose? ((connector expose-transport-metrics-mixin)
                              (metric    symbol))
  (declare (notinline member))
  (member metric (connector-expose connector) :test #'eq))

(defmethod (setf connector-expose?) ((new-value (eql nil))
                                     (connector expose-transport-metrics-mixin)
                                     (metric    symbol))
  (removef (connector-expose connector) metric)
  new-value)

(defmethod (setf connector-expose?) ((new-value t)
                                     (connector expose-transport-metrics-mixin)
                                     (metric    symbol))
  (pushnew metric (connector-expose connector))
  new-value)

(defmethod (setf connector-expose) ((new-value symbol)
                                    (connector expose-transport-metrics-mixin))
  (setf (connector-expose connector) (list new-value))
  new-value)
