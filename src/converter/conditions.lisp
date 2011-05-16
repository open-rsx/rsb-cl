;;; conditions.lisp ---
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

(in-package :rsb.converter)

(define-condition conversion-error (rsb:rsb-error)
  ((wire-schema :initarg  :wire-schema
		:type     symbol
		:reader   conversion-error-wire-schema
		:documentation
		""))
  (:report
   (lambda (condition stream)
     (format stream "~@<A conversion to or from wire-schema ~S failed.~@:>"
	     (conversion-error-wire-schema condition))))
  (:documentation
   "This condition class can be used as a superclass for
conversion-related condition classes."))

(define-condition wire->domain-conversion-error (conversion-error)
  ((encoded     :initarg  :encoded
		:type     t
		:reader   conversion-error-encoded
		:documentation
		"")
   (domain-type :initarg  :domain-type
		:type     t
		:reader   conversion-error-domain-type
		:documentation
		""))
  (:report
   (lambda (condition stream)
     (format stream "~@<The wire data ~S (in ~S schema) could not be ~
converted to domain type ~S.~@:>"
	     (conversion-error-encoded     condition)
	     (conversion-error-wire-schema condition)
	     (conversion-error-domain-type condition))))
  (:documentation
   "This error is signaled when wire data cannot be converted to a
domain object."))

(define-condition domain->wire-conversion-error (conversion-error)
  ((domain-object :initarg  :domain-object
		  :type     t
		  :reader   conversion-error-domain-object
		  :documentation
		  "")
   (wire-type     :initarg  :wire-type
		  :type     t
		  :reader   conversion-error-wire-type
		  :documentation
		  ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<The domain object ~S could not be converted to ~
a wire-type ~S representation using the wire-schema ~S.~@:> "
	     (conversion-error-domain-object condition)
	     (conversion-error-wire-type     condition)
	     (conversion-error-wire-schema   condition))))
  (:documentation
   "This error is signaled when a domain object cannot be converted to
a wire-type representation."))
