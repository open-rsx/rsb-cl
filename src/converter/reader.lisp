;;; reader.lisp ---
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

(defmethod wire->domain ((wire-schema (eql :reader))
			 (encoded     string)  ;; TODO octet-vector?
			 (domain-type class)) ;; TODO could also be symbol and standard-object
  (with-standard-io-syntax
    (read-from-string encoded)))

(defmethod wire->domain :around ((wire-schema (eql :reader))
				 (encoded     string)
				 (domain-type class))
  (let ((expected-type (class-name domain-type))
	(result        (call-next-method)))
    (unless (typep result expected-type)
      (error 'wire->domain-conversion-error
	     :wire-format wire-schema
	     :encoded     encoded
	     :domain-type domain-type
	     :format-control "~@<The value is not ..."))
    result)) ;; TODO also type-error?

(defmethod domain->wire ((wire-schema   (eql :reader))
			 (domain-object t)
			 (wire-type     (eql 'string))) ;; TODO octet-vector?
  (with-standard-io-syntax
    (prin1-to-string domain-object)))

;; roundtrip test
;; (wire->domain :reader (domain->wire :reader :boo 'string) (find-class 'symbol))
