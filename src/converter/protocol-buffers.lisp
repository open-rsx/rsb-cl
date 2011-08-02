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

(defmethod wire->domain? ((converter   (eql :protocol-buffer))
			  (wire-data   simple-array)
			  (wire-schema symbol))
  (let* ((descriptor (pb:find-descriptor wire-schema
					 :error? nil))
	 (class      (when descriptor
		       (pb:descriptor-class descriptor))))
    (when class
      (values converter (class-name class)))))

(defmethod domain->wire? ((converter     (eql :protocol-buffer))
			  (domain-object standard-object))
  (when (pb:message-descriptor domain-object)
    (values
     converter 'octet-vector (class-name (class-of domain-object)))))

(defmethod wire->domain ((converter   (eql :protocol-buffer))
			 (wire-data   simple-array)
			 (wire-schema symbol))
  (check-type wire-data octet-vector)

  (let* ((descriptor (pb:find-descriptor (string wire-schema)))
	 (class      (pb:descriptor-class descriptor)))
    (nth-value 0 (pb:unpack wire-data class))))

(defmethod domain->wire ((converter     (eql :protocol-buffer))
			 (domain-object standard-object))
  (let* ((descriptor  (pb:message-descriptor domain-object))
	 (wire-schema (pb:descriptor-qualified-name descriptor)))
   (values (pb:pack* domain-object) wire-schema)))
