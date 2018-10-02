;;;; in-push-connector.lisp --- An in-direction, push-based connector for spread.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `in-push-connector' class

(defclass in-push-connector (in-connector
                             broadcast-processor
                             error-policy-mixin)
  ()
  (:metaclass connector-class)
  (:direction :in-push)
  (:documentation
   "Push-style event receiving for the Spread transport."))

(register-connector :spread :in-push 'in-push-connector)

(defmethod handle ((sink in-push-connector) (data bus-notification))
  ;; Turn the assembled notification in DATA into an `event' instance
  ;; and dispatch to handlers.
  (dispatch sink (notification->event sink data :whatever)))
