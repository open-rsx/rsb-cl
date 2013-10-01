;;;; out-route-configurator.lisp --- Configurator class for out-direction processing.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defclass out-route-configurator (configurator)
  ()
  (:default-initargs
   :direction :out)
  (:documentation
   "Instances of this class configure out-direction connectors and an
event processor for sending of events."))

(defmethod collect-processor-mixins append ((configurator out-route-configurator))
  '(broadcast-processor))

;;; Connectors

(defmethod notify ((configurator out-route-configurator)
                   (connector    t)
                   (action       (eql :connector-added)))
  (let+ (((&accessors-r/o (processor configurator-processor)) configurator))
    (call-next-method)
    (log1 :trace configurator "Connecting ~S -> ~S" processor connector)
    (push connector (handlers processor))))

(defmethod notify ((configurator out-route-configurator)
                   (connector    t)
                   (action       (eql :connector-removed)))
  (let+ (((&accessors-r/o (processor configurator-processor)) configurator))
    (log1 :trace configurator "Disconnecting ~S -> ~S" processor connector)
    (removef (handlers processor) connector)
    (call-next-method)))
