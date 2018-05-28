;;;; types.lisp --- Mixin classes for processor classes.
;;;;
;;;; Copyright (C) 2016, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

;;; Subscription-related types

(defstruct (subscription (:constructor nil) (:predicate nil) (:copier nil))
  (scope nil :type scope :read-only t))

(declaim (inline subscribed))
(defstruct (subscribed
             (:include subscription)
             (:constructor subscribed (scope))
             (:predicate nil) (:copier nil)))

(declaim (inline unsubscribed))
(defstruct (unsubscribed
             (:include subscription)
             (:constructor unsubscribed (scope))
             (:predicate nil) (:copier nil)))

;;; Dispatching-related types

(declaim (inline scope-and-event))
(defstruct (scope-and-event
             (:constructor scope-and-event (scope event))
             (:copier nil))
  (scope nil :type scope :read-only t)
  (event nil :type t     :read-only t))
