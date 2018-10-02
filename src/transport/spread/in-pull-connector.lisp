;;;; in-pull-connector.lisp --- An in-direction, pull-based connector for spread.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defclass in-pull-connector (in-connector
                             error-handling-pull-receiver-mixin)
  ((queue :type     lparallel.queue:queue
          :reader   queue
          :initform (lparallel.queue:make-queue)
          :documentation
          "Stores notifications as they arrive via the message bus."))
  (:metaclass connector-class)
  (:direction :in-pull)
  (:documentation
   "Pull-style event receiving for the Spread transport."))

(register-connector :spread :in-pull 'in-pull-connector)

(defmethod handle ((connector in-pull-connector)
                   (data      bus-notification))
  ;; Put DATA into the queue of CONNECTOR for later retrieval.
  (lparallel.queue:push-queue data (queue connector)))

(defmethod receive-notification ((connector in-pull-connector)
                                 (block?    (eql nil)))
  ;; Extract and return one event from the queue maintained by
  ;; CONNECTOR, if there are any. If there are no queued events,
  ;; return nil.
  (lparallel.queue:try-pop-queue (queue connector)))

(defmethod receive-notification ((connector in-pull-connector)
                                 (block?    t))
  ;; Extract and return one event from the queue maintained by
  ;; CONNECTOR, if there are any. If there are no queued events,
  ;; block.
  (lparallel.queue:pop-queue (queue connector)))

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
