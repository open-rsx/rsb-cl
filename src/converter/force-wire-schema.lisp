;;; force-wire-schema.lisp --- A converter that sets a given wire-schema.
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

(defmethod find-converter-class ((spec (eql :force-wire-schema)))
  (find-class 'force-wire-schema))

(defclass force-wire-schema ()
  ((wire-schema :initarg  :wire-schema
		:type     keyword
		:accessor converter-wire-schema
		:initform :bytes
		:documentation
		"Stores the wire-schema that should be used when
performing domain->wire \"conversions\"."))
  (:documentation
   "Instances of this class do not perform any changes when converting
between wire-data and domain-data but set a given wire-schema when
producing wire-data, wire-schema pairs."))

(defmethod wire->domain? ((converter   force-wire-schema)
			  (wire-data   t)
			  (wire-schema t))
  "The converter can handle arbitrary wire-data."
  (values converter t))

(defmethod domain->wire? ((converter     force-wire-schema)
			  (domain-object t))
  "The converter can handle arbitrary domain objects."
  (bind (((:accessors-r/o
	   (wire-schema converter-wire-schema)) converter))
    (values converter t wire-schema)))

(defmethod wire->domain ((converter   force-wire-schema)
			 (wire-data   t)
			 (wire-schema t))
  "The wire-data is not modified."
  wire-data)

(defmethod domain->wire ((converter     force-wire-schema)
			 (domain-object t))
  "The domain object is not modified, but the configured wire-schema
is set."
  (bind (((:accessors-r/o
	   (wire-schema converter-wire-schema)) converter))
    (values domain-object wire-schema)))

(defmethod print-object ((object force-wire-schema) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (converter-wire-schema object))))
