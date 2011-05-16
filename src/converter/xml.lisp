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


;;; Wire -> domain conversions
;;

(defmethod wire->domain ((wire-schema (eql :xml))
			 (encoded     stp:document)
			 (domain-type (eql 'stp:document)))
  "No conversion is required if ENCODED already is a `stp:document'."
  encoded)

;; TODO domain could also be an instance; cxml-location allows this
(defmethod wire->domain ((wire-schema (eql :xml))
			 (encoded     string)
			 (domain-type symbol))
  (let* ((document (cxml:parse encoded (stp:make-builder)))
	 (root     (stp:document-element document)))
    (cxml-location:xml-> root domain-type)))

(defmethod wire->domain ((wire-schema (eql :xml))
			 (encoded     simple-array)
			 (domain-type symbol))
  (check-type encoded octet-vector "an octet-vector")

  (let* ((document (cxml:parse-octets encoded (stp:make-builder)))
	 (root     (stp:document-element document)))
    (cxml-location:xml-> root domain-type)))


;;; Domain -> wire conversions
;;

(defmethod domain->wire ((wire-schema   (eql :xml))
			 (domain-object stp:document)
			 (wire-type     (eql 'stp:document)))
  "No conversion is required if DOMAIN-OBJECT already is a
`stp:document'"
  domain-object)

(defmethod domain->wire ((wire-schema   (eql :xml))
			 (domain-object t)
			 (wire-type     (eql 'stp:document)))
  (let* ((root     (stp:make-element "object")) ;; TODO this is somehow arbitrary
	 (document (stp:make-document root)))
    (cxml-location:->xml domain-object root :any) ;(type-of domain-object))
    document))

(defmethod domain->wire ((wire-schema   (eql :xml))
			 (domain-object t)
			 (wire-type     (eql 'string)))
  (stp:serialize
   (domain->wire wire-schema domain-object 'stp:document)
   (cxml:make-string-sink)))

;; TODO control ?xml declaration
;; (cxml:make-string-sink :omit-xml-declaration-p t)
;; TODO control indent
;; For indent
;; (apply #'cxml:make-string-sink (when indent? '(:indentation 2)))

(defmethod domain->wire ((wire-schema   (eql :xml))
			 (domain-object t)
			 (wire-type     (eql 'octet-vector)))
  (stp:serialize
   (domain->wire wire-schema domain-object 'stp:document)
   (cxml:make-octet-vector-sink)))

