;;; fundamental.lisp --- Converters for fundamental types.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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
