;;;; out-connector.lisp --- Out-direction connector for socket transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass out-connector (error-handling-sender-mixin
                         restart-notification-sender-mixin
                         timestamping-sender-mixin
                         connector)
  ()
  (:metaclass connector-class)
  (:direction :out)
  (:documentation
   "Out-direction connector for socket transport."))

(register-connector :socket :out 'out-connector)

(defmethod handle ((connector out-connector)
                   (event     event))
  (send-notification connector (event->notification connector event)))

(defmethod event->notification ((connector out-connector)
                                (event     event))
  ;; Delegate conversion to `event->notifications'. The primary
  ;; purpose of this method is performing the conversion with restarts
  ;; installed.
  (event->notification* connector event))

(defmethod send-notification ((connector    out-connector)
                              (notification notification))
  (handle (connector-bus connector) notification))
