;;; protocol.lisp --- Wire <-> domain conversion protocol.
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


;;; Default behavior for the converter info protocol.
;;

(defmethod no-applicable-method ((function (eql (fdefinition 'wire->domain?)))
				 &rest args)
  "If there is no method on `wire->domain?' for a given combination of
converter, wire-data and wire-schema, the converter is cannot handle
the data."
  (declare (ignore args))
  nil)

(defmethod no-applicable-method ((function (eql (fdefinition 'domain->wire?)))
				 &rest args)
  "If there is no method on `domain->wire?' for a given pair of
converter and data, the converter cannot handle the data."
  (declare (ignore args))
  nil)


;;; Converter implementations
;;

(dynamic-classes:define-findable-class-family converter
    "Converters are things that do something.")
