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

(defgeneric wire->domain (wire-schema wire-data domain-type)
  (:documentation
   "Decode WIRE-DATA into a Lisp object of type DOMAIN-TYPE (or at
least designated by DOMAIN-TYPE) using the interpretation described by
WIRE-SCHEMA."))

;; TODO wire-schema may not be needed here
(defgeneric domain->wire (wire-schema domain-object wire-type)
  (:documentation
   "Encode the Lisp object DOMAIN-OBJECT into the wire representation
described by the pair (WIRE-TYPE WIRE-SCHEMA) and return the
constructed wire representation object. The type of the returned
object depends on WIRE-TYPE."))


;;; Default behavior
;;

;; (defmethod no-applicable-method ((function (fdefinition 'wire->domain))
;;				 &rest args)
;;   "DOC"
;;   )

;; (defmethod wire->domain ((wire-schema t)
;;			 (encoded     t)
;;			 (domain-type symbol))
;;   (wire->domain wire-schema encoded (find-class domain-type)))


;;; Converter implementations
;;

(dynamic-classes:define-findable-class-family converter
    "Converters are things that do something.")
