;;; conditions.lisp --- Conditions used in the spread transport implementation.
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

(in-package :rsb.transport.spread)


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


;;; Fragmentation-related conditions
;;


(define-condition assembly-problem (condition)
  ((assembly :initarg  :assembly
	     :reader   assembly-problem-assembly
	     :documentation
	     "The assembly instance in which the problem occurred."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Something happened in assembly ~A.~@:>"
	     (assembly-problem-assembly condition))))
  (:documentation
   "Instance of subclasses of this condition are signaled if a problem
occurs during the assembly of fragments into complete events."))

(define-condition fragment-problem (assembly-problem)
  ((fragment :initarg  :fragment
	     :reader   assembly-problem-fragment
	     :documentation
	     "The notification representing the fragment that caused
the problem."))
  (:report
   (lambda (condition stream)
     (bind (((:accessors-r/o
	      (assembly assembly-problem-assembly)
	      (fragment assembly-problem-fragment)) condition))
      (format stream "~@<The fragment ~D of event ~/rsb::print-id/ ~
caused a problem in assembly ~A.~@:>"
	      (rsb.protocol::notification-data-part fragment)
	      (assembly-id assembly)
	      assembly))))
  (:documentation
   "Instance of subclasses of this condition are signaled if a
fragment causes a problem in an assembly."))

(define-condition invalid-fragment-id (fragment-problem
				       warning)
  ()
  (:report
   (lambda (condition stream)
     (bind (((:accessors-r/o
	      (assembly assembly-problem-assembly)
	      (fragment assembly-problem-fragment)) condition))
       (format stream "~@<Received illegal fragment ~D of event ~
~/rsb::print-id/ with ~D parts in assembly ~A.~@:>"
	       (rsb.protocol::notification-data-part fragment)
	       (assembly-id assembly)
	       (length (assembly-fragments assembly))
	       assembly))))
  (:documentation
   "This warning is signaled when an attempt is made to add a fragment
with an invalid part id to an assembly."))

(define-condition duplicate-fragment (fragment-problem
				      warning)
  ()
  (:report
   (lambda (condition stream)
     (bind (((:accessors-r/o
	      (assembly assembly-problem-assembly)
	      (fragment assembly-problem-fragment)) condition))
       (format stream "~@<Received fragment ~D of event ~
~/rsb::print-id/ more than once in assembly ~A.~@:>"
	       (rsb.protocol::notification-data-part fragment)
	       (assembly-id assembly)
	       assembly))))
  (:documentation
   "This warning is signaled when an attempt is made to add a fragment
to an assembly that has already been added."))
