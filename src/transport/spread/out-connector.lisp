;;;; out-connector.lisp --- Out-direction connector for Spread transport.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defclass out-connector (restart-notification-sender-mixin
                         error-handling-sender-mixin
                         timestamping-sender-mixin
                         connector)
  ((max-fragment-size   :initarg  :max-fragment-size
                        :type     positive-fixnum
                        :reader   connector-max-fragment-size
                        :initform 100000
                        :documentation
                        "The maximum payload size that may be send in a single notification. The value of this options has to be chosen such that the combined sizes of payload and envelope data of notifications remain below the maximum message size allowed by spread.")
   (scope->groups-cache :reader   connector-%scope->groups-cache
                        :initform (make-scope->groups-cache)))
  (:metaclass connector-class)
  (:direction :out)
  (:options
   (:max-fragment-size &slot))
  (:documentation
   "A connector for sending data over Spread."))

(register-connector :spread :out 'out-connector)

(defmethod notify ((recipient out-connector)
                   (subject   scope)
                   (action    t))
  (notify recipient t action))

(defmethod handle ((sink out-connector) (data event))
  (let+ (((&structure-r/o connector- (cache %scope->groups-cache)) sink)
         (group-names   (scope->groups (event-scope data) cache))
         (notifications (event->notification sink data)))
    ;; Due to large events being fragmented into multiple
    ;; notifications, we obtain a list of notifications here.
    (send-notification sink (cons group-names notifications))))

(defmethod event->notification ((connector out-connector) (event event))
  ;; Delegate conversion to `event->notifications'. The primary
  ;; purpose of this method is performing the conversion with restarts
  ;; installed.
  (event->notifications
   connector event (connector-max-fragment-size connector)))

(defmethod send-notification ((connector out-connector) (notification cons))
  ;; Send each notification using `send-message'. The primary purpose
  ;; of this method is sending the notifications with restarts
  ;; installed.
  (let+ (((&accessors-r/o (connection connector-connection)) connector)
         ((group-names . notifications) notification))
    (iter (for notification in notifications)
          (let* ((buffer       (pb:pack* notification))
                 (notification (make-wire-notification
                                buffer (length buffer))))
            (send-message connection group-names notification)))))
