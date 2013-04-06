;;; conditions.lisp --- Conditions used in the converter module.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.converter)

(define-condition conversion-error (rsb-error
				    chainable-condition)
  ((wire-schema :initarg  :wire-schema
		:type     symbol
		:reader   conversion-error-wire-schema
		:documentation
		"This wire-schema to or from which the failed
conversion would have converted."))
  (:default-initargs
   :references (append
		(default-references 'rsb-error)
		(list (documentation-ref/rsb-glossary "converter"))))
  (:report
   (lambda (condition stream)
     (format stream "~@<A conversion to or from wire-schema ~S~
failed~/more-conditions::maybe-print-cause/~@:>"
	     (conversion-error-wire-schema condition)
	     condition)))
  (:documentation
   "This condition class can be used as a superclass for
conversion-related condition classes."))

(define-condition wire->domain-conversion-error (conversion-error)
  ((encoded     :initarg  :encoded
		:type     t
		:reader   conversion-error-encoded
		:documentation
		"The wire-data that could not be converted into a
domain object.")
   (domain-type :initarg  :domain-type
		:type     t
		:reader   conversion-error-domain-type
		:documentation
		"The type of the domain object object that would have
been produced by a successful conversion."))
  (:report
   (lambda (condition stream)
     (let+ (((&values data shortened?)
	     (maybe-shorten-sequence
	      (conversion-error-encoded condition))))
       (format stream "~@<The wire data ~S~:[~; ...~] (in ~S ~
wire-schema) could not be converted to domain type ~
~S~/more-conditions::maybe-print-cause/~@:>"
	       data shortened?
	       (conversion-error-wire-schema condition)
	       (conversion-error-domain-type condition)
	       condition))))
  (:documentation
   "This error is signaled when wire data cannot be converted to a
domain object."))

(define-condition domain->wire-conversion-error (conversion-error)
  ((domain-object :initarg  :domain-object
		  :type     t
		  :reader   conversion-error-domain-object
		  :documentation
		  "The domain object that could not be converter into
a wire representation.")
   (wire-type     :initarg  :wire-type
		  :type     t
		  :reader   conversion-error-wire-type
		  :documentation
		  "The type of the wire-data that would have been
produced by a successful conversion."))
  (:report
   (lambda (condition stream)
     (format stream "~@<The domain object ~S could not be converted to ~
a wire-type ~S representation using the wire-schema ~
~S~/more-conditions::maybe-print-cause/~@:> "
	     (conversion-error-domain-object condition)
	     (conversion-error-wire-type     condition)
	     (conversion-error-wire-schema   condition)
	     condition)))
  (:documentation
   "This error is signaled when a domain object cannot be converted to
a wire-type representation."))

(define-condition no-converter (rsb-error)
  ((datum :initarg  :datum
          :reader   no-converter-datum
          :documentation
          "")
   (tried :initarg  :tried
          :type     list
          :reader   no-converter-tried
          :initform nil
          :documentation
          ""))
  (:default-initargs
   :datum      (missing-required-initarg 'no-converter :datum)
   :references (append (default-references 'rsb-error)
		       (list (documentation-ref/rsb-manual
			      "Troubleshooting" "Missing Converters"))))
  (:documentation
   "Instances of subclasses of this condition class are signaled when
no converter is able to perform a certain conversion."))

(define-condition no-wire->domain-converter (no-converter)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<No converter could handle the wire-data ~S (in ~S ~
wire-schema). Tried ~{~A~^, ~_~}~@:>"
             (no-converter-datum condition)
             :TODO-WIRE-SCHEMA
             (no-converter-tried condition))))
  (:documentation
   "This error is signaled when a wire->domain conversion cannot be
performed because a suitable converter cannot be found."))

(define-condition no-domain->wire-converter (no-converter)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<No converter could handle the domain-object ~
~S. Tried ~{~A~^, ~_~}~@:>"
             (no-converter-datum condition)
             (no-converter-tried condition))))
  (:documentation
   "This error is signaled when a domain->wire conversion cannot be
performed because a suitable converter cannot be found."))
