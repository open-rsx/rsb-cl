;;;; conditions.lisp --- Conditions used in the transport module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport)

(define-condition connector-construction-failed (rsb-error
                                                 chainable-condition)
  ((name      :initarg  :name
              :type     keyword
              :reader   connector-construction-failed-name
              :documentation
              "Name of the connector class that should have been
used.")
   (direction :initarg  :direction
              :type     keyword
              :reader   connector-construction-failed-direction
              :documentation
              "Desired direction of the connector instance that should
have been constructed.")
   (args      :initarg  :args
              :type     list
              :reader connector-construction-failed-args
              :documentation
              "Arguments for the connector instance that should have
been constructed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to construct ~A connector for ~
direction ~A~@[ with options~{~_~2T~16A~^: ~@<~@;~S~>~^,~}~].~
~/more-conditions::maybe-print-cause/~@:>"
             (connector-construction-failed-name      condition)
             (connector-construction-failed-direction condition)
             (connector-construction-failed-args      condition)
             condition)))
  (:documentation
   "This error is signaled when the construction of a connector
instance fails."))

;;;

(define-condition connection-closed (condition)
  ((connection :initarg  :connection
               :reader   connection-closed-connection
               :documentation
               "Stores the connection which has been closed."))
  (:default-initargs
   :connection (missing-required-initarg 'connection-closed :connection))
  (:report
   (lambda (condition stream)
     (format stream "~@<The connection ~A is closed.~@:>"
             (connection-closed-connection condition))))
  (:documentation
   "This condition and subclasses are signaled when a connection is
closed."))

(define-condition connection-unexpectedly-closed (communication-error
                                                  connection-closed
                                                  chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The connection ~A has been closed ~
unexpectedly.~/more-conditions::maybe-print-cause/~@:>"
             (connection-closed-connection condition)
             condition)))
  (:documentation
   "This error is signaled when a connection is closed
unexpectedly. In contrast to the more generic `connection-closed'
condition, this is considered an error."))

;;; Conversion-related errors
;;;
;;; No suitable convert, encoding and decoding errors.

(define-condition no-suitable-converter (connector-construction-failed)
  ((wire-type  :initarg  :wire-type
               :reader   connector-construction-failed-wire-type
               :documentation
               "Stores the wire-type for which a converter could not
be found.")
   (candidates :initarg  :candidates
               :type     list
               :reader   connector-construction-failed-candidates
               :documentation
               "Stores an a list of converter candidates of the
form (WIRE-TYPE . CONVERTER)."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to construct ~A connector for direction ~
~A~@[ with options~{~_~2T~16A: ~@<~@;~S~>~^,~}~]~_because no converter ~
for wire-type ~S could be found ~:[without any candidates (empty ~
converter list)~;~:*in candidate list ~{~A~^, ~_~}~].~@:>"
             (connector-construction-failed-name       condition)
             (connector-construction-failed-direction  condition)
             (connector-construction-failed-args       condition)
             (connector-construction-failed-wire-type  condition)
             (connector-construction-failed-candidates condition))))
  (:documentation
   "This error is signaled when the construction of a connector
instance fails because the list of available converters does not
contain a suitable one for the requested connector."))

(define-condition decoding-error (rsb-error
                                  simple-condition
                                  chainable-condition)
  ((encoded :initarg  :encoded
            :type     octet-vector
            :reader   decoding-error-encoded
            :documentation
            "The encoded data, for which the decoding failed."))
  (:report
   (lambda (condition stream)
     (let+ (((&values data shortened?)
             (maybe-shorten-sequence (decoding-error-encoded condition))))
       (format stream "~@<The encoded data ~_~{~2,'0X~^ ~}~:[~; ...~] ~
~_could not be decoded ~
~/rsb::maybe-print-explanation/~/more-conditions::maybe-print-cause/~@:>"
               (coerce data 'list) shortened?
               condition
               condition))))
  (:documentation
   "This error is signaled when decoding one or more notifications
into an `event' instance fails."))

(define-condition encoding-error (rsb-error
                                  simple-condition
                                  chainable-condition)
  ((event :initarg  :event
          :reader   encoding-error-event
          :documentation
          "The event for which the encoding failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The event ~A could not be encoded into an ~
octet-vector~/rsb::maybe-print-explanation/~/more-conditions::maybe-print-cause/~@:>"
             (encoding-error-event      condition)
             condition
             condition)))
  (:documentation
   "This error is signaled when encoding an `event' instance into one
or more notifications fails."))
