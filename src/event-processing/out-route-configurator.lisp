;;;; out-route-configurator.lisp --- Configurator class for out-direction processing.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defclass out-route-configurator (configurator)
  ()
  (:default-initargs
   :direction :out)
  (:documentation
   "Configure out-direction connectors and a processor for a client.

    The client generally is an event-sending participant."))

(defmethod collect-processor-mixins append ((configurator out-route-configurator))
  '(error-policy-handler-mixin
    restart-handler-mixin
    broadcast-processor))

;;; Connectors

(defmethod notify ((configurator out-route-configurator)
                   (connector    t)
                   (action       (eql :connector-added)))
  (let+ (((&structure-r/o configurator- processor) configurator))
    (call-next-method)
    (log:trace "~@<~A is connecting ~A -> ~A~@:>"
               configurator processor connector)
    (push connector (handlers processor))))

(defmethod notify ((configurator out-route-configurator)
                   (connector    t)
                   (action       (eql :connector-removed)))
  (let+ (((&structure-r/o configurator- processor) configurator))
    (log:trace "~@<~A is disconnecting ~A -> ~A~@:>"
               configurator processor connector)
    (removef (handlers processor) connector :count 1)
    (call-next-method)))
