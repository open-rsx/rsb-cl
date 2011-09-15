;;; fundamental-numbers.lisp --- Fundamental converters for number types.
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

(define-simple-converter (:fundamental-double :double double-float)
    ((binio:decode-double-le wire-data))
  ((nth-value 1 (binio:encode-double-le domain-object))))

(define-simple-converter (:fundamental-float :float single-float)
    ((binio:decode-float-le wire-data))
  ((nth-value 1 (binio:encode-float-le domain-object))))

(define-simple-converter (:fundamental-int32 :int32 (signed-byte 32)
			  :data-type-class integer)
    ((binio:decode-sint32-le wire-data))
  ((nth-value 1 (binio:encode-sint32-le domain-object))))

(define-simple-converter (:fundamental-int64 :int64 (signed-byte 64)
			  :data-type-class integer)
    ((binio:decode-sint64-le wire-data))
  ((nth-value 1 (binio:encode-sint64-le domain-object))))

(define-simple-converter (:fundamental-uint32 :uint32 (unsigned-byte 64)
			  :data-type-class integer)
    ((binio:decode-uint32-le wire-data))
  ((nth-value 1 (binio:encode-uint32-le domain-object))))

(define-simple-converter (:fundamental-uint64 :uint64 (unsigned-byte 64)
			  :data-type-class integer)
    ((binio:decode-uint64-le wire-data))
  ((nth-value 1 (binio:encode-uint64-le domain-object))))

;; (define-simple-converter (:fundamental-bool :bool boolean)
;;   ((binio:decode-uint32-le wire-data))
;;   ((binio:encode-uint32-le domain-object)))
