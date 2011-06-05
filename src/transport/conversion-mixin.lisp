;;; conversion-mixin.lisp --- A mixin for converter selection.
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

(defclass conversion-mixin ()
  ((converter :initarg  :converter
	      :accessor connector-converter
	      :documentation
	      "A converter to which the actual conversion work is
delegated."))
  (:default-initargs
   :converter (missing-required-initarg
	       'conversion-mixin :converter))
  (:documentation
   "This mixin adds methods on `domain->wire' and `wire->domain' for
the subclass which delegate the conversion tasks to a stored
converter."))

(defmethod domain->wire ((connector     conversion-mixin)
			 (domain-object t))
  "Delegate conversion of DOMAIN-OBJECT to the converter stored in
CONNECTOR."
  (domain->wire (connector-converter connector) domain-object))

(defmethod wire->domain ((connector   conversion-mixin)
			 (wire-data   t)
			 (wire-schema t))
  "Delegate the conversion of WIRE-DATA, WIRE-SCHEMA to the converter
stored in CONNECTOR."
  (wire->domain (connector-converter connector) wire-data wire-schema))

(defmethod print-object ((object conversion-mixin) stream)
  (bind (((:accessors-r/o (converter connector-converter)) object)
	 (sequence? (typep converter 'sequence)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~:[~S~;(~D)~]"
	      sequence? (if sequence? (length converter) converter)))))
