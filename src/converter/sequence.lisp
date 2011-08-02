;;; sequence.lisp --- Sequences of alternative converters.
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


;;; Sequence "converter"
;;
;; Sequences are interpreted as a prioritized list of alternative
;; converters. That is, the first converter that is able to perform a
;; requested conversion is selected and used.

(defmethod wire->domain? ((converter   sequence)
			  (wire-data   t)
			  (wire-schema t))
  "Check whether any child can handle the requested conversion."
  (iter (for child in converter)
	(bind (((:values use? domain-type)
		(wire->domain? child wire-data wire-schema)))
	  (when use?
	    (return (values use? domain-type))))))

(defmethod domain->wire? ((converter     sequence)
			  (domain-object t))
  "Check whether any child can handle the requested conversion."
  (iter (for child in converter)
	(bind (((:values use? wire-type wire-schema)
		(domain->wire? child domain-object)))
	  (when use?
	    (return (values use? wire-type wire-schema))))))

(defmethod wire->domain ((converter   sequence)
			 (wire-data   t)
			 (wire-schema t))
  (let ((child (wire->domain? converter wire-data wire-schema)))
    (unless child
      (error "~@<No converter could handle the wire-data ~S (in ~S ~
wire-schema). Tried ~{~A~^, ~_~}~@:>"
	     wire-data wire-schema converter)) ;;; TODO(jmoringe): more precise condition
    (wire->domain child wire-data wire-schema)))

(defmethod domain->wire ((converter     sequence)
			 (domain-object t))
  (let ((child (domain->wire? converter domain-object)))
    (unless child
      (error "~@<No converter could handle the domain-object ~S. Tried ~
~{~A~^, ~_~}~@:>" domain-object converter)) ;;; TODO(jmoringe): more precise condition
    (domain->wire child domain-object)))
