;;; fundamental.lisp --- Converters for fundamental types.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.converter)


;;; Special fundamental converters
;;

(defconstant +no-value+ '%no-value
  "This object is used to represent the absence of a value.")

(deftype no-value ()
  "Instances of this type represent the absence of a value."
  '(eql %no-value))

(define-simple-converter (:fundamental-void :void no-value
			  :wire-type       (simple-array (unsigned-byte 8) (0))
			  :data-type-class (eql +no-value+))
    (+no-value+)
  ((make-array 0 :element-type '(unsigned-byte 8))))

(define-simple-converter (:fundamental-null t t
			  :wire-type       t
			  :wire-type-class t)
    (wire-data)
  (domain-object))


;;; Numeric fundamental types
;;

(define-simple-converter (:fundamental-bool :bool boolean
			  :data-type-class t)
  ((case (aref wire-data 0)
     (0 nil)
     (1 t)
     (t (error "~@<Invalid value: ~D~@:>"
	       (aref wire-data 0)))))
  ((octet-vector (if domain-object 1 0))))

(macrolet
    ((define-number-converter (wire-schema data-type size accessor
					   &key data-type-class)
       (let ((name (format-symbol :keyword "FUNDAMENTAL-~A" wire-schema)))
	 `(define-simple-converter (,name ,wire-schema ,data-type
				    ,@(when data-type-class
					`(:data-type-class ,data-type-class)))
	    ((,accessor wire-data 0))
	    ((let ((result (make-octet-vector ,size)))
	       (setf (,accessor result 0) domain-object)
	       result))))))

  (define-number-converter :uint32 (unsigned-byte 32) 4 ub32ref/le
    :data-type-class integer)
  (define-number-converter :int32  (signed-byte 32)   4 sb32ref/le
    :data-type-class integer)
  (define-number-converter :uint64 (unsigned-byte 64) 8 ub64ref/le
    :data-type-class integer)
  (define-number-converter :int64  (signed-byte 64)   8 sb64ref/le
    :data-type-class integer)
  (define-number-converter :float  single-float       4 ieee-single-ref/le)
  (define-number-converter :double double-float       8 ieee-double-ref/le))


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
