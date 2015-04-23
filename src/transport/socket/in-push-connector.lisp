;;;; in-push-connector.lisp --- In-direction, push-style socket connector.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass in-push-connector (error-policy-handler-mixin
                             restart-handler-mixin
                             in-connector)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "This class implements in-direction, push-style communication over
a socket."))

(register-connector :socket :in-push 'in-push-connector)

(defmethod handle ((connector in-push-connector)
                   (data      notification))
  ;; TODO(jmoringe): condition translation?
  (when-let ((event (notification->event connector data :undetermined)))
    (dispatch connector event)))
