;;;; in-pull-connector.lisp --- An in-direction, pull-based connector for spread.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defmethod find-transport-class ((spec (eql :spread-in-pull)))
  (find-class 'in-pull-connector))

(defclass in-pull-connector (error-handling-pull-receiver-mixin
                             in-connector)
  ()
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "This class implements pull-style event receiving for the Spread
transport."))

(defmethod notify ((connector in-pull-connector)
                   (scope     scope)
                   (action    (eql :attached)))
  (let+ (((&structure-r/o connector- connection) connector)
         ((&values ref-count group-count promise)
          (ref-group (connector-connection connector) (scope->group scope)
                     :waitable? t)))
    ;; When this was the initial reference to the initial group of the
    ;; connection, attach CONNECTOR.
    (when (and (= ref-count 1) (= group-count 1))
      (notify connector t :attached))
    ;; If necessary, wait for the Spread group joining operation to
    ;; complete.
    (iter (until (lparallel:fulfilledp promise))
          (receive-message connection nil))))

(defmethod notify ((connector in-pull-connector)
                   (scope     scope)
                   (action    (eql :detached)))
  (let+ (((&structure-r/o connector- connection) connector)
         ((&values &ign group-count promise)
          (unref-group (connector-connection connector) (scope->group scope)
                       :waitable? t)))
    ;; If necessary, wait for the Spread group leaving operation to
    ;; complete.
    (iter (until (lparallel:fulfilledp promise))
          (receive-message connection nil))
    ;; If this was the final reference to the final group of the
    ;; connection, detach CONNECTOR.
    (when (zerop group-count)
      (notify connector t :detached))))

(defmethod emit ((connector in-pull-connector) (block? t))
  ;; Maybe block until a notification is received. Try to convert into
  ;; an event and return the event in case of success. In blocking
  ;; mode, wait for the next notification.
  (iter (let* ((notification (receive-notification connector block?))
               (event (when notification
                        (notification->event
                         connector notification :undetermined))))

          ;; Due to fragmentation of large events into multiple
          ;; notifications, non-blocking receive mode and error
          ;; handling policies, we may not obtain an `event' instance
          ;; from the notification.
          (when event
            (dispatch connector event))
          (when (or event (not block?))
            (return event)))))
