;;;; in-push-connector.lisp --- In-direction, push-style socket connector.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defmethod find-transport-class ((spec (eql :socket-in-push)))
  (find-class 'in-push-connector))

(defclass in-push-connector (error-policy-handler-mixin
                             restart-handler-mixin
                             in-connector)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "This class implements in-direction, push-style communication over
a socket."))

(defmethod handle ((connector in-push-connector)
                   (data      notification))
  ;; TODO(jmoringe): condition translation?
  (when-let ((event (notification->event connector data :undetermined)))
    (dispatch connector event)))
