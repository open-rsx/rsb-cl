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

(defmethod wire->domain ((converter   (eql :xml-dom))
			 (wire-data   stp:document)
			 (wire-schema t))
  (let ((root (stp:document-element wire-data)))
    (cxml-location:xml-> root wire-schema)))

(defmethod wire->domain ((converter   (eql :xml-string))
			 (wire-data   string)
			 (wire-schema t))
  (cxml:parse wire-data (stp:make-builder)))

(defmethod wire->domain ((converter   (eql :xml-bytes))
			 (wire-data   simple-array)
			 (wire-schema t))
  (check-type wire-data octet-vector "an octet-vector")

  (cxml:parse-octets wire-data (stp:make-builder)))


;;; Domain -> wire conversions
;;

(defmethod domain->wire ((converter     (eql :xml-dom))
			 (domain-object t))
  (let* ((root     (stp:make-element "object")) ;; TODO this is somehow arbitrary
	 (document (stp:make-document root)))
    (cxml-location:->xml domain-object root :any)
    (values document domain-object)))

(defmethod domain->wire ((converter     (eql :xml-string))
			 (domain-object stp:document))
  (values
   (stp:serialize domain-object (cxml:make-string-sink))
   :TODO))

(defmethod domain->wire ((converter     (eql :xml-bytes))
			 (domain-object t))
  (values
   (stp:serialize domain-object (cxml:make-octet-vector-sink))
   :TODO))

;; TODO control ?xml declaration
;; (cxml:make-string-sink :omit-xml-declaration-p t)
;; TODO control indent
;; For indent
;; (apply #'cxml:make-string-sink (when indent? '(:indentation 2)))
