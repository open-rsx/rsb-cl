;;;; out-connector.lisp --- Out-direction connector for socket transport.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defmethod find-transport-class ((spec (eql :socket-out)))
  (find-class 'out-connector))

(defclass out-connector (error-handling-sender-mixin
                         restart-notification-sender-mixin
                         connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Out-direction connector for socket transport."))

(defmethod handle ((connector out-connector)
                   (event     event))
  (send-notification connector (event->notification connector event)))

(defmethod event->notification ((connector out-connector)
                                (event     event))
  "Delegate conversion to `event->notifications'. The primary purpose
of this method is performing the conversion with restarts installed."
  (event->notification connector event))

(defmethod send-notification ((connector    out-connector)
                              (notification notification))
  (handle (connector-bus connector) notification))
