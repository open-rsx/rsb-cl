;;; protocol.lisp --- Wire <-> domain conversion protocol.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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


;;; Converter protocol
;;

(defgeneric wire->domain (converter wire-data wire-schema)
  (:documentation
   "Decode WIRE-DATA into a Lisp object using an interpretation
according to WIRE-SCHEMA and CONVERTER. Return the decoded Lisp
object.

Example:
RSB.CONVERTER> (wire->domain :fundamental-string #(102 111 111) :string)
=> \"foo\""))

(defgeneric domain->wire (converter domain-object)
  (:documentation
   "Encode the Lisp object DOMAIN-OBJECT into the wire representation
associated to CONVERTER. Return two values: the constructed wire
representation and the wire-schema.

Example:
RSB.CONVERTER> (domain->wire :fundamental-string \"foo\")
=> #(102 111 111) :string"))


;;; Converter info protocol
;;

(defgeneric wire->domain? (converter wire-data wire-schema)
  (:documentation
   "Return non-nil if CONVERTER can be used to convert WIRE-DATA into
a Lisp object using the interpretation designated by WIRE-SCHEMA. If
such a conversion is possible, return two values: CONVERTER and the
type of the Lisp object that the conversion would produce.

Example:
RSB.CONVERTER> (wire->domain? :fundamental-string #(102 111 111) :string)
=> :fundamental-string 'string"))

(defgeneric domain->wire? (converter domain-object)
  (:documentation
   "Return non-nil if CONVERTER can be used to convert DOMAIN-OBJECT
to its wire-type. If such a conversion is possible, return three
values: CONVERTER, the wire-type and the wire-schema the conversion
would produce.

Example:
RSB.CONVERTER> (domain->wire? :fundamental-string \"foo\")
=> :fundamental-string 'rsb:octet-vector :string"))


;;; Default behavior
;;

(defmethod no-applicable-method ((function (eql (fdefinition 'wire->domain?)))
				 &rest args)
  "If there is no method on `wire->domain?' for a given combination of
converter, wire-data and wire-schema, the converter cannot handle the
data."
  (declare (ignore args))
  nil)

(defmethod no-applicable-method ((function (eql (fdefinition 'domain->wire?)))
				 &rest args)
  "If there is no method on `domain->wire?' for a given pair of
converter and data, the converter cannot handle the data."
  (declare (ignore args))
  nil)

(defmethod wire->domain :around ((converter   t)
				 (wire-data   t)
				 (wire-schema t))
  "Install \"retry\" and \"use-value\" restarts around the call to the
next `wire->domain' method."
  (iter
    (restart-case
	(handler-bind
	    (((and error (not wire->domain-conversion-error))
	      #'(lambda (condition)
		  (error 'wire->domain-conversion-error
			 :wire-schema wire-schema
			 :encoded     wire-data
			 :domain-type :undetermined
			 :cause       condition))))
	  (return (call-next-method)))
      (retry ()
	:report (lambda (stream)
		  (format stream "~@<Retry converting ~S (in ~S ~
schema) using converter ~A.~@:>"
			  wire-data wire-schema converter)) nil)
      (use-value (value)
	:report      (lambda (stream)
		       (format stream "~@<Supply a replacement value ~
to use instead of converting ~S (in ~S schema) using converter ~
~A.~@:>"
			       wire-data wire-schema converter))
	:interactive (lambda ()
		       (format *query-io* "Enter replacement value (evaluated): ")
		       (list (read *query-io*)))
	(return value)))))

(defmethod domain->wire :around ((converter     t)
				 (domain-object t))
  "Install \"retry\" and \"use-value\" restarts around the call to the
next `domain->wire' method."
  (iter
    (restart-case
	(handler-bind
	    (((and error (not domain->wire-conversion-error))
	      #'(lambda (condition)
		  (error 'domain->wire-conversion-error
			 :wire-schema   :undetermined
			 :domain-object domain-object
			 :wire-type     :undetermined
			 :cause         condition))))
	  (return (call-next-method)))
      (retry ()
	:report (lambda (stream)
		  (format stream "~@<Retry converting ~A using converter ~A.~@:>"
			  domain-object converter))
	nil)
      (use-value (value)
	:report      (lambda (stream)
		       (format stream "~@<Supply a replacement value ~
to use instead of converting ~A using converter ~A.~@:>"
			       domain-object converter))
	:interactive (lambda ()
		       (format *query-io* "Enter replacement value (evaluated): ")
		       (list (read *query-io*)))
	(return value)))))


;;; Converter implementations
;;

(dynamic-classes:define-findable-class-family converter
    "Converters are things that do something.")
