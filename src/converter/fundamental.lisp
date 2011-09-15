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


;;; Special fundamental converters
;;

(define-simple-converter (:fundamental-void :void null
			  :wire-type       (simple-array (unsigned-byte 8) (0))
			  :data-type-class (eql nil))
    (nil)
  ((make-array 0 :element-type '(unsigned-byte 8))))

(define-simple-converter (:fundamental-null t t
			  :wire-type       t
			  :wire-type-class t)
    (wire-data)
  (domain-object))


;;; Sequence-like fundamental types
;;

(define-simple-converter
    (:fundamental-ascii-string :ascii-string string)
    ((sb-ext:octets-to-string wire-data :external-format :ascii))
  ((sb-ext:string-to-octets domain-object :external-format :ascii)))

(define-simple-converter
    (:fundamental-utf-8-string :utf-8-string string)
    ((sb-ext:octets-to-string wire-data :external-format :utf-8))
  ((sb-ext:string-to-octets domain-object :external-format :utf-8)))

(define-simple-converter
    (:fundamental-bytes :bytes (vector (unsigned-byte 8))
			:data-type-class simple-array)
    (wire-data)
  ((coerce domain-object 'octet-vector)))
