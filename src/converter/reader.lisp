;;; reader.lisp --- A converter that uses the Lisp reader/printer.
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

(defmethod wire->domain ((converter   (eql :reader))
			 (wire-data   string)
			 (wire-schema symbol))
  (with-standard-io-syntax
    (read-from-string wire-data)))

(defmethod wire->domain :around ((converter   (eql :reader))
				 (wire-data   string)
				 (wire-schema symbol))
  (let ((expected-type wire-schema)
	(result        (call-next-method)))
    (unless (typep result expected-type)
      (error 'wire->domain-conversion-error
	     :encoded     wire-data
	     :wire-schema wire-schema
	     :domain-type expected-type
	     :format-control "~@<The value is not ~A is not of the expected type ~A.~@:>"
	     :format-arguments `(,result ,expected-type)))
    result)) ;; TODO also type-error?

(defmethod domain->wire ((converter     (eql :reader))
			 (domain-object t))
  (values
   (with-standard-io-syntax
     (prin1-to-string domain-object))
   (type-of domain-object)))
