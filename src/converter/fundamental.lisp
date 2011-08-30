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

(macrolet
    ((define-fundamental-converter ((name wire-schema data-type
				     &key
				     (wire-type       'octet-vector)
				     (wire-type-class 'simple-array)
				     (data-type-class data-type      data-type-class-supplied?))
				    (&body wire->domain)
				    (&body domain->wire))
       `(progn
	  (defmethod wire->domain? ((converter   (eql ,name))
				    (wire-data   ,wire-type-class)
				    (wire-schema (eql ,wire-schema)))
	    (when (typep wire-data ',wire-type)
	      (values converter ',data-type)))

	  (defmethod domain->wire? ((converter     (eql ,name))
				    (domain-object ,data-type-class))
	    ,(if data-type-class-supplied?
		 `(when (typep domain-object ',data-type)
		    (values converter 'octet-vector ,wire-schema))
		 `(values converter 'octet-vector ,wire-schema)))

	  (defmethod wire->domain ((converter   (eql ,name))
				   (wire-data   ,wire-type-class)
				   (wire-schema (eql ,wire-schema)))
	    (check-type wire-data ,wire-type)

	    ,@wire->domain)

	  (defmethod domain->wire ((converter     (eql ,name))
				   (domain-object ,data-type-class))
	    (check-type domain-object ,data-type)

	    (values (progn ,@domain->wire) ,wire-schema)))))

  ;; Special fundamental converters
  (define-fundamental-converter (:fundamental-void :void null
				 :wire-type       (simple-array (unsigned-byte 8) (0))
				 :data-type-class (eql nil))
    (nil)
    ((binio:make-octet-vector 0)))
  (define-fundamental-converter (:fundamental-null t t
				 :wire-type       t
				 :wire-type-class t)
    (wire-data)
    (domain-object))

  ;; Sequence-like fundamental types
  (define-fundamental-converter
      (:fundamental-ascii-string :ascii-string string)
    ((sb-ext:octets-to-string wire-data :external-format :ascii))
    ((sb-ext:string-to-octets domain-object :external-format :ascii)))
  (define-fundamental-converter
      (:fundamental-utf-8-string :utf-8-string string)
    ((sb-ext:octets-to-string wire-data :external-format :utf-8))
    ((sb-ext:string-to-octets domain-object :external-format :utf-8)))
  (define-fundamental-converter
      (:fundamental-bytes :bytes (vector (unsigned-byte 8))
       :data-type-class simple-array)
    (wire-data)
    ((coerce domain-object 'octet-vector)))

  ;; Numeric fundamental types
  (define-fundamental-converter (:fundamental-double :double double-float)
    ((binio:decode-double-le wire-data))
    ((nth-value 1 (binio:encode-double-le domain-object))))
  (define-fundamental-converter (:fundamental-float :float single-float)
    ((binio:decode-float-le wire-data))
    ((nth-value 1 (binio:encode-float-le domain-object))))
  (define-fundamental-converter (:fundamental-int32 :int32 (signed-byte 32)
				 :data-type-class integer)
    ((binio:decode-sint32-le wire-data))
    ((nth-value 1 (binio:encode-sint32-le domain-object))))
  (define-fundamental-converter (:fundamental-int64 :int64 (signed-byte 64)
				 :data-type-class integer)
    ((binio:decode-sint64-le wire-data))
    ((nth-value 1 (binio:encode-sint64-le domain-object))))
  (define-fundamental-converter (:fundamental-uint32 :uint32 (unsigned-byte 64)
				 :data-type-class integer)
    ((binio:decode-uint32-le wire-data))
    ((nth-value 1 (binio:encode-uint32-le domain-object))))
  (define-fundamental-converter (:fundamental-uint64 :uint64 (unsigned-byte 64)
				 :data-type-class integer)
    ((binio:decode-uint64-le wire-data))
    ((nth-value 1 (binio:encode-uint64-le domain-object)))))
