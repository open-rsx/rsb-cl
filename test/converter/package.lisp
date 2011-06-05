;;; package.lisp --- Package definition for unit tests of the converter module.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>
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

(cl:in-package :cl-user)

(defpackage :rsb.converter.test
  (:use
   :cl
   :alexandria
   :bind
   :lift

   :rsb
   :rsb.converter

   :rsb.test)

  (:documentation
   "This package contains unit tests for the converter module."))

(in-package :rsb.converter.test)

(deftestsuite converter-root (root)
  ()
  (:function
   (octetify (value)
     (coerce value 'octet-vector)))
  (:documentation
   "Root unit test suite for the converter module."))

(defmacro define-basic-converter-test-cases (converter cases)
  "Emit basic test cases for CONVERTER."
  (let ((suite-name (symbolicate converter "-ROOT")))
    `(progn

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test methods on `wire->domain?' for the `~(~A~)' converter."
			  converter))
	 wire->domain-applicability

	 (ensure-cases (wire-data wire-schema domain-object)
	     ,cases
	   (let ((result (wire->domain? ,converter wire-data wire-schema)))
	     (ensure-same result ,converter
			  :ignore-multiple-values? t))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test methods on `domain->wire?' for the `~(~A~)' converter."
			  converter))
	 domain->wire-applicability

	 (ensure-cases (wire-data wire-schema domain-object)
	     ,cases
	   (let ((result (domain->wire? ,converter domain-object)))
	     (ensure-same result (values ,converter wire-schema)))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Roundtrip test for the `~(~A~)' converter."
			  converter))
	 roundtrip

	 (ensure-cases (wire-data wire-schema domain-object)
	     ,cases
	   (bind (((:values encoded encoded-wire-schema)
		   (domain->wire ,converter domain-object))
		  (decoded (wire->domain ,converter encoded encoded-wire-schema)))
	     (ensure-same domain-object decoded
			  :test #'equal)))))))
