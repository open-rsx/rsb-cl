;;; protocol-buffers.lisp --- Converter for protocol buffer wire format.
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

(defmethod wire->domain ((wire-schema (eql :protocol-buffer))
			 (encoded     simple-array)
			 (domain-type symbol))
  (check-type encoded octet-vector)

  (nth-value 0 (pb:unpack encoded domain-type)))

(defmethod domain->wire ((wire-schema   (eql :protocol-buffer))
			 (domain-object standard-object)
			 (wire-type     (eql 'octet-vector)))
  (pb::pack1 domain-object))


;;;
;;

;; (defmethod wire->domain ((wire-schema (eql :protocol-buffer))
;;			 (encoded     string)
;;			 (domain-type class)) ;; TODO could also be symbol and standard-object
;;   (let ((bytes (base64:base64-string-to-usb8-array encoded)))
;;     (wire->domain wire-format bytes domain-type)))
;;
;; (defmethod domain->wire ((wire-schema   (eql :protocol-buffer))
;;			 (domain-object standard-object)
;;			 (wire-type     (eql 'base64-string)))
;;   (base64:usb8-array-to-base64-string
;;    (domain->wire :protocol-buffer domain-object 'octet-vector)))
