;;; xml.lisp --- XML converter based on cxml-location.
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


;;; Domain <-> DOM conversion
;;

(defmethod wire->domain? ((converter   (eql :xml-dom))
			  (wire-data   stp:document)
			  (wire-schema t))
  (values converter t))

(defmethod domain->wire? ((converter     (eql :xml-dom))
			  (domain-object t))
  (values converter 'stp:document (type-of domain-object)))

(defmethod wire->domain ((converter   (eql :xml-dom))
			 (wire-data   stp:document)
			 (wire-schema t))
  (let ((root (stp:document-element wire-data)))
    (cxml-location:xml-> root wire-schema)))

(defmethod domain->wire ((converter     (eql :xml-dom))
			 (domain-object t))
  (let* ((root     (stp:make-element "payload"))
	 (document (stp:make-document root)))
    (cxml-location:->xml domain-object root :any)
    (values document (type-of domain-object))))


;;; DOM <-> string conversion
;;

(defmethod wire->domain? ((converter   (eql :xml-string))
			  (wire-data   string)
			  (wire-schema (eql :utf-8-xml)))
  (values converter 'stp:document))

(defmethod domain->wire? ((converter     (eql :xml-string))
			  (domain-object stp:document))
  (values converter 'string :utf-8-xml))

(defmethod wire->domain ((converter   (eql :xml-string))
			 (wire-data   string)
			 (wire-schema (eql :utf-8-xml)))
  (cxml:parse wire-data (stp:make-builder)))

(defmethod domain->wire ((converter     (eql :xml-string))
			 (domain-object stp:document))
  (values
   (stp:serialize domain-object (cxml:make-string-sink
				 :omit-xml-declaration-p t))
   :utf-8-xml))


;;; DOM <-> bytes conversions
;;

(defmethod wire->domain? ((converter   (eql :xml-bytes))
			  (wire-data   simple-array)
			  (wire-schema (eql :bytes-xml)))
  (when (typep wire-data 'octet-vector)
    (values converter 'stp:document)))

(defmethod domain->wire? ((converter     (eql :xml-bytes))
			  (domain-object stp:document))
  (values converter 'string :bytes-xml))

(defmethod wire->domain ((converter   (eql :xml-bytes))
			 (wire-data   simple-array)
			 (wire-schema (eql :bytes-xml)))
  (check-type wire-data octet-vector "an octet-vector")

  (cxml:parse-octets wire-data (stp:make-builder)))

(defmethod domain->wire ((converter     (eql :xml-bytes))
			 (domain-object stp:document))
  (values
   (coerce
    (stp:serialize domain-object (cxml:make-octet-vector-sink
				  :omit-xml-declaration-p t))
    'octet-vector)
   :bytes-xml))
