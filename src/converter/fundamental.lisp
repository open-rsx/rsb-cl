;;; fundamental.lisp --- Converters for fundamental types.
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


;;; Null converter
;;

(defmethod wire->domain? ((converter   (eql :fundamental-null))
			  (wire-data   t)
			  (wire-schema t))
  (values converter t))

(defmethod domain->wire? ((converter     (eql :fundamental-null))
			  (domain-object t))
  (values converter t t))

(defmethod wire->domain ((converter   (eql :fundamental-null))
			 (wire-data   t)
			 (wire-schema t))
  wire-data)

(defmethod domain->wire ((converter     (eql :fundamental-null))
			 (domain-object t))
  (values domain-object t))


;;; ASCII string converter
;;

(defmethod wire->domain? ((converter   (eql :fundamental-ascii-string))
			  (wire-data   simple-array)
			  (wire-schema (eql :ascii-string)))
  (when (typep wire-data '(vector (unsigned-byte 8)))
    (values converter 'string)))

(defmethod domain->wire? ((converter     (eql :fundamental-ascii-string))
			  (domain-object string))
  (values converter 'octet-vector :ascii-string))

(defmethod wire->domain ((converter   (eql :fundamental-ascii-string))
			 (wire-data   simple-array)
			 (wire-schema (eql :ascii-string)))
  (check-type wire-data (vector (unsigned-byte 8)))

  (sb-ext:octets-to-string wire-data :external-format :ascii))

(defmethod domain->wire ((converter     (eql :fundamental-ascii-string))
			 (domain-object string))
  (values
   (sb-ext:string-to-octets domain-object :external-format :ascii)
   :ascii-string))


;;; UTF-8 string converter
;;

(defmethod wire->domain? ((converter   (eql :fundamental-utf-8-string))
			  (wire-data   simple-array)
			  (wire-schema (eql :utf-8-string)))
  (when (typep wire-data '(vector (unsigned-byte 8)))
    (values converter 'string)))

(defmethod domain->wire? ((converter     (eql :fundamental-utf-8-string))
			  (domain-object string))
  (values converter 'octet-vector :utf-8-string))

(defmethod wire->domain ((converter   (eql :fundamental-utf-8-string))
			 (wire-data   simple-array)
			 (wire-schema (eql :utf-8-string)))
  (check-type wire-data (vector (unsigned-byte 8)))

  (sb-ext:octets-to-string wire-data :external-format :utf-8))

(defmethod domain->wire ((converter     (eql :fundamental-utf-8-string))
			 (domain-object string))
  (values
   (sb-ext:string-to-octets domain-object :external-format :utf-8)
   :utf-8-string))


;;; Bytes converter
;;

(defmethod wire->domain? ((converter   (eql :fundamental-bytes))
			  (wire-data   simple-array)
			  (wire-schema (eql :bytes)))
  (when (typep wire-data 'octet-vector)
    (values converter 'string)))

(defmethod domain->wire? ((converter     (eql :fundamental-bytes))
			  (domain-object simple-array))
  (when (typep domain-object '(vector (unsigned-byte 8)))
    (values converter 'octet-vector :bytes)))

(defmethod wire->domain ((converter   (eql :fundamental-bytes))
			 (wire-data   simple-array)
			 (wire-schema (eql :bytes)))
  (check-type wire-data octet-vector)

  wire-data)

(defmethod domain->wire ((converter     (eql :fundamental-bytes))
			 (domain-object simple-array))
  (check-type domain-object (vector (unsigned-byte 8)))

  (values (coerce domain-object 'octet-vector) :bytes))
