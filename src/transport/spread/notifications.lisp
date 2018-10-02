;;;; notifications.lisp --- Notifications used in the transport.spread module.
;;;;
;;;; Copyright (C) 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; Notification-related types

(deftype message-length-limit ()
  `(integer 0 (,network.spread:+maximum-message-data-length+)))

;;; Connection notifications

(defstruct (destined-wire-notification
            (:include wire-notification)
            (:constructor make-destined-wire-notification
                (destination buffer end)))
  ;; Spread group or groups to which the message is sent.
  (destination nil :type list #|of string|# :read-only t))

;;; Bus notifications

(defstruct (bus-notification
            (:constructor nil) (:predicate nil) (:copier nil))
  ;; The incoming or outgoing notification.
  (notification nil :type rsb.protocol:notification :read-only t)
  ;; Complete (i.e. potentially assembled from multiple fragments)
  ;; wire-data of the payload.
  (wire-data    nil :type octet-vector              :read-only t))

(defstruct (incoming-notification
            (:include bus-notification)
            (:constructor make-incoming-notification (notification wire-data))
            (:predicate nil) (:copier nil)))

(defstruct (outgoing-notification
            (:include bus-notification)
            (:constructor make-outgoing-notification
                (scope destination notification wire-data))
            (:predicate nil) (:copier nil))
  ;; The scope of the event that gave rise to the notification. Used
  ;; when dispatching to in-process in-connectors.
  (scope       nil :type scope              :read-only t)
  ;; Spread groups to which the notification should be sent.
  (destination nil :type list #|of string|# :read-only t))
