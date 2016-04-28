;;;; in-pull-connector.lisp --- An in-direction, pull-based connector for spread.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defclass in-pull-connector (error-handling-pull-receiver-mixin
                             in-connector)
  ()
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "Pull-style event receiving for the Spread transport."))

(register-connector :spread :in-pull 'in-pull-connector)

(defmethod notify ((recipient in-pull-connector)
                   (subject   scope)
                   (action    (eql :attached)))
  (let+ (((&structure-r/o connector- connection) recipient)
         ((&values ref-count group-count promise)
          (ref-group (connector-connection recipient) (scope->group subject)
                     :waitable? t)))
    ;; When this was the initial reference to the initial group of the
    ;; connection, attach RECIPIENT.
    (when (and (= ref-count 1) (= group-count 1))
      (notify recipient t :attached))
    ;; If necessary, wait for the Spread group joining operation to
    ;; complete.
    (iter (until (lparallel:fulfilledp promise))
          (receive-message connection nil))))

(defmethod notify ((recipient in-pull-connector)
                   (subject   scope)
                   (action    (eql :detached)))
  (let+ (((&structure-r/o connector- connection) recipient)
         ((&values &ign group-count promise)
          (unref-group (connector-connection recipient) (scope->group subject)
                       :waitable? t)))
    ;; If necessary, wait for the Spread group leaving operation to
    ;; complete.
    (iter (until (lparallel:fulfilledp promise))
          (receive-message connection nil))
    ;; If this was the final reference to the final group of the
    ;; connection, detach RECIPIENT.
    (when (zerop group-count)
      (notify recipient t :detached))))

(defmethod emit ((source in-pull-connector) (block? t))
  ;; Maybe block until a notification is received. Try to convert into
  ;; an event and return the event in case of success. In blocking
  ;; mode, wait for the next notification.
  (iter (let* ((notification (receive-notification source block?))
               (event (when notification
                        (notification->event
                         source notification :undetermined))))

          ;; Due to fragmentation of large events into multiple
          ;; notifications, non-blocking receive mode and error
          ;; handling policies, we may not obtain an `event' instance
          ;; from the notification.
          (when event
            (dispatch source event))
          (when (or event (not block?))
            (return event)))))
