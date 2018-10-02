;;;; package.lisp --- Package definition for unit tests of the transport.spread module.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.spread.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions
   #:lift

   #:nibbles

   #:rsb
   #:rsb.transport
   #:rsb.transport.spread

   #:rsb.test
   #:rsb.transport.test)

  (:import-from #:rsb.transport.spread
   #:normalize-daemon-endpoint

   #:*scope->groups-cache*
   #:*scope->groups-cache-max-size*
   #:make-scope->groups-cache
   #:scope->group
   #:scope->groups/no-cache
   #:scope->groups

   #:assembly
   #:assembly-complete?
   #:assembly-concatenated-data

   #:assembly-pool-count
   #:merge-fragment

   #:assembly-pool
   #:pruning-assembly-pool

   #:make-notification
   #:split-notification

   #:connection

   #:in-connector

   #:in-pull-connector

   #:out-connector)

  (:documentation
   "This package contains unit tests for the transport.spread
module"))

(cl:in-package #:rsb.transport.spread.test)

;;; Root test suite

(deftestsuite transport-spread-root (transport-root)
  ((spread-port cl-rsb-system::*spread-port*)
   common-args)
  (:setup
   (setf common-args `(:schema    :spread
                       :port      ,spread-port
                       :converter :fundamental-null)))
  (:documentation
   "Root unit test suite for the transport.spread module."))

;;; Test utilities

(defvar *simple-group-name*
  (concatenate 'string "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" '(#\Nul)))

(defun octetify (data)
  (etypecase data
    (string   (sb-ext:string-to-octets data))
    (sequence (coerce data 'octet-vector))
    (t        data)))

(defun a-notification (sequence-number data &key (scope "/foo") wire-schema)
  (let ((event-id (make-instance 'rsb.protocol:event-id
                                 :sender-id       (uuid:uuid-to-byte-array
                                                   (uuid:make-null-uuid))
                                 :sequence-number sequence-number)))
    (apply #'make-instance 'rsb.protocol:notification
           :scope    (octetify scope)
           :event-id event-id
           :data     data
           (when wire-schema
             (list :wire-schema (octetify wire-schema))))))

(defun a-fragment (sequence-number length id data)
  (make-instance 'rsb.protocol:fragmented-notification
                 :notification   (a-notification sequence-number data)
                 :num-data-parts length
                 :data-part      id))

(defun collect-fragments (generator)
  (loop :for fragment = (funcall generator)
        :while fragment
        :collect fragment))

(defun make-event* (data)
  (let ((event (make-event "/foo" (octetify data))))
    (setf (event-origin event)          (uuid:make-null-uuid)
          (event-sequence-number event) 0)
    event))

(defun call-with-connection (thunk
                             &key
                             port
                             (name (format nil "~D" port)))
  (let ((connection (make-instance 'connection :name name)))
    (unwind-protect
         (funcall thunk connection)
      (detach connection))))

(defmacro with-connection ((connection-var &rest args &key port name)
                           &body body)
  (declare (ignore port name))
  `(call-with-connection (lambda (,connection-var) ,@body) ,@args))
