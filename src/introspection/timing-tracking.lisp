;;;; timing-tracking.lisp --- Track timing properties of remote processes, hosts.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; This file contains the following classes:
;;;;
;;;; `tracked-quantity'
;;;;
;;;;   Stores a series of measurements for a single quantity and
;;;;   provides simple statistical analysis: median, mean, variance.
;;;;
;;;; `timing-tracker' (aka Ghetto NTP)
;;;;
;;;;   An instance of this class consumes events sent by a particular
;;;;   remote process to the local process in response to previous
;;;;   events sent in the opposite direction. The remote process
;;;;   annotates reply events with send and receive timestamps of the
;;;;   corresponding request events.
;;;;
;;;;   Applying the basic offset estimation algorithm of the Network
;;;;   Time Protocol allows estimating the difference between the
;;;;   local clock and the remote clock as well as the mean event
;;;;   transport latency based on a set of such events.
;;;;
;;;;   See
;;;;   http://en.wikipedia.org/wiki/Network_Time_Protocol#Clock_synchronization_algorithm
;;;;
;;;;   Uses two `tracked-quantity' instances to track clock offsets
;;;;   and latencies.

(cl:in-package #:rsb.introspection)

;;; `tracked-quantity'

(defstruct (tracked-quantity
             (:constructor make-tracked-quantity
                           (&key name (capacity 100) (history '())))
             (:copier nil))
  "Instances of this track measured values of a quantity over time."
  (name     nil :type (or null string))
  ;; Stores the maximum number of offset samples to keep for median,
  ;; mean, etc. calculation.
  (capacity 100 :type positive-integer)
  ;; Stores offset samples for mean offset calculation.
  (history  '() :type list #| of real |#))

(defun tracked-quantity-value (quantity)
  (let+ (((&structure-r/o tracked-quantity- history) quantity))
    (when (> (length history) 3)
      (values (median history) (mean history) (variance history)))))

(defun tracked-quantity-add (quantity value)
  (let+ (((&structure tracked-quantity- capacity history) quantity))
    (push value history)
    (when (> (length history) capacity)
      (setf (cdr (nthcdr (1- capacity) history)) '())))
  quantity)

(defun tracked-quantity-reset (quantity)
  (setf (tracked-quantity-history quantity) '()))

(defmethod print-items:print-items append ((object tracked-quantity))
  `((:name        ,(tracked-quantity-name object),            "~S")
    (:value       ,(tracked-quantity-value object)            " ~:[n/a~:;~:*~A~]" ((:after :name)))
    (:num-entries ,(length (tracked-quantity-history object)) " (~D)"             ((:after :value)))))

(defmethod print-object ((object tracked-quantity) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/print-items:format-print-items/"
            (print-items:print-items object))))

;;; `timing-tracker'

(defclass timing-tracker ()
  ((clock-offset :type     tracked-quantity
                 :reader   timing-tracker-%clock-offset
                 :initform (make-tracked-quantity :name "clock-offset")
                 :documentation
                 "Stores the clock offset estimation as a
                 `tracked-quantity' instance.")
   (latency      :type     tracked-quantity
                 :reader   timing-tracker-%latency
                 :initform (make-tracked-quantity :name "latency")
                 :documentation
                 "Stores the latency estimation as a
                 `tracked-quantity' instance."))
  (:documentation
   "Instances of this class consume events with timestamp annotations
    and estimate the clock offset and latency between the local host
    and remote hosts based on a bounded log of events."))

(defmethod timing-tracker-clock-offset ((tracker timing-tracker))
  (tracked-quantity-value (timing-tracker-%clock-offset tracker)))

(defmethod timing-tracker-latency ((tracker timing-tracker))
  (tracked-quantity-value (timing-tracker-%latency tracker)))

(defmethod timing-tracker-to-local-clock ((tracker timing-tracker)
                                          (time    local-time:timestamp)
                                          &key
                                          allow-future?)
  (let+ (((&flet maybe-transform (time)
            (if-let ((offset (timing-tracker-clock-offset tracker)))
              (let+ (((&values seconds remainder) (floor (- offset))))
                (local-time:adjust-timestamp time
                  (:offset :sec  seconds)
                  (:offset :nsec (floor remainder 1/1000000000))))
              time)))
         ((&flet ensure-not-future (time)
            (extremum (list (local-time:now) time)
                      #'local-time:timestamp<)))
         (localized (maybe-transform time)))
    (if allow-future?
        localized
        (ensure-not-future localized))))

(defmethod rsb.ep:handle ((sink timing-tracker) (data event))
  ;; This is the actual clock offset and latency estimation. See
  ;; description above for explanation and reference.
  (let+ (((&structure-r/o timing-tracker- %clock-offset %latency) sink))
    (when-let* ((t0      (timestamp data :request.send))
                (t1      (timestamp data :request.receive))
                (t2      (timestamp data :send))
                (t3      (timestamp data :receive))
                (diff1   (local-time:timestamp-difference t1 t0))
                (diff2   (local-time:timestamp-difference t3 t2))
                (offset  (/ (- diff1 diff2) 2))
                (latency (/ (+ diff1 diff2) 2)))
      (tracked-quantity-add %clock-offset offset)
      (tracked-quantity-add %latency      latency))))

(defmethod rsb.ep:handle ((sink timing-tracker) (data (eql :reset)))
  (tracked-quantity-reset (timing-tracker-%clock-offset sink))
  (tracked-quantity-reset (timing-tracker-%latency sink)))
