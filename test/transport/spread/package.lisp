;;;; package.lisp --- Package definition for unit tests of the transport.spread module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

   #:event->notifications

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
  ((spread-port (asdf:component-property
                 (asdf:find-system :rsb-transport-spread/test) :spread-port))
   common-args)
  (:setup
   (setf common-args `(:port      ,spread-port
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

(defun make-fragment (sequence-number length id data)
  (let* ((event-id     (make-instance
                        'rsb.protocol:event-id
                        :sender-id       (uuid:uuid-to-byte-array
                                          (uuid:make-null-uuid))
                        :sequence-number sequence-number))
         (notification (make-instance 'rsb.protocol:notification
                                      :event-id event-id
                                      :data     data)))
    (make-instance 'rsb.protocol:fragmented-notification
                   :notification   notification
                   :num-data-parts length
                   :data-part      id)))

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
