;;; conditions.lisp --- Conditions used in the transport module.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.transport)

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
direction ~A~@[ with options~{~_~2T~16A: ~@<~@;~S~>~^,~}~].~
~/rsb::maybe-print-cause/~@:>"
	     (connector-construction-failed-name      condition)
	     (connector-construction-failed-direction condition)
	     (connector-construction-failed-args      condition)
	     (chainable-condition-cause               condition))))
  (:documentation
   "This error is signaled when the construction of a connector
instance fails."))


;;; Generic encoding and decoding errors
;;

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
     (bind (((:values data shortened?)
	     (maybe-shorten-sequence (decoding-error-encoded condition))))
       (format stream "~@<The encoded data ~_~{~2,'0X~^ ~}~:[~; ...~] ~
~_could not be decoded ~
~/rsb::maybe-print-explanation/~/rsb::maybe-print-cause/~@:>"
	       (coerce data 'list) shortened?
	       condition
	       (chainable-condition-cause condition)))))
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
octet-vector~/rsb::maybe-print-explanation/~/rsb::maybe-print-cause/~@:>"
	     (encoding-error-event      condition)
	     condition
	     (chainable-condition-cause condition))))
  (:documentation
   "This error is signaled when encoding an `event' instance into one
or more notifications fails."))
