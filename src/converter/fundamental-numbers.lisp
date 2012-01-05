;;; fundamental-numbers.lisp --- Fundamental converters for number types.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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
