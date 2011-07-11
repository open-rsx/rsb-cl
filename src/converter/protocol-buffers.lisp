;;; protocol-buffers.lisp --- Converter for protocol buffer wire schemas.
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

(defmethod wire->domain ((converter   (eql :protocol-buffer))
			 (wire-data   simple-array)
			 (wire-schema symbol))
  (check-type wire-data octet-vector)

  (let ((type (pb::proto-type-name->lisp-type-symbol
	       (string wire-schema))))
    (nth-value 0 (pb:unpack wire-data type))))

(defmethod domain->wire ((converter     (eql :protocol-buffer))
			 (domain-object standard-object))
  (values (pb:pack* domain-object) (type-of domain-object)))
