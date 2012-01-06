;;; package.lisp --- Package definition for unit tests of the converter module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.DE>
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

(cl:defpackage :rsb.converter.test
  (:use
   :cl
   :alexandria
   :let-plus
   :lift

   :rsb
   :rsb.converter

   :rsb.test)

  (:export
   :converter-root
   :octetify

   :define-basic-converter-test-cases)

  (:documentation
   "This package contains unit tests for the converter module."))

(cl:in-package :rsb.converter.test)

(deftestsuite converter-root (root)
  ()
  (:function
   (octetify (value)
     (coerce value 'octet-vector)))
  (:documentation
   "Root unit test suite for the converter module."))

(defmacro define-basic-converter-test-cases ((converter
					      &key
					      (domain-test 'equalp))
					     cases)
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
	   (cond
	     ((member wire-data '(:error :not-applicable)))
	     ((eq domain-object :not-applicable)
	      (let ((result (wire->domain? ,converter wire-data wire-schema)))
		(ensure-null result)))
	     (t
	      (let ((result (wire->domain? ,converter wire-data wire-schema)))
		(ensure-same result ,converter
			     :ignore-multiple-values? t))))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test methods on `domain->wire?' for the `~(~A~)' converter."
			  converter))
	 domain->wire-applicability

	 (ensure-cases (wire-data wire-schema domain-object)
	     ,cases
	   (cond
	     ((member domain-object '(:error :not-applicable)))
	     ((eq wire-data :not-applicable)
	      (let ((result (domain->wire? ,converter domain-object)))
		(ensure-null result)))
	     (t
	      (let ((result (domain->wire? ,converter domain-object)))
		(ensure-same result (values ,converter wire-schema)))))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test methods on `wire->domain' for the `~(~A~)' converter."
			  converter))
	 wire->domain-smoke

	 (ensure-cases (wire-data wire-schema domain-object)
	     ,cases
	   (cond
	     ((member wire-data '(:error :not-applicable)))
	     ((member domain-object '(:error :not-applicable))
	      (ensure-condition 'error
		(wire->domain ,converter wire-data wire-schema)))
	     (t
	      (let ((result (wire->domain ,converter wire-data wire-schema)))
		(ensure-same result domain-object
			     :test (function ,domain-test)))))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Test methods on `domain->wire' for the `~(~A~)' converter."
			  converter))
	 domain->wire-smoke

	 (ensure-cases (wire-data wire-schema domain-object)
	     ,cases
	   (cond
	     ((member domain-object '(:error :not-applicable)))
	     ((member wire-data '(:error :not-applicable))
	      (ensure-condition 'error
		(domain->wire ,converter domain-object)))
	     (t
	      (let ((result (domain->wire ,converter domain-object)))
		(ensure-same result (values wire-data wire-schema)
			     :test #'equalp))))))

       (addtest (,suite-name
		 :documentation
		 ,(format nil "Roundtrip test for the `~(~A~)' converter."
			  converter))
	 roundtrip

	 (ensure-cases (wire-data wire-schema domain-object)
	     ,cases
	   (unless (or (member wire-data '(:error :not-applicable))
		       (member domain-object '(:error :not-applicable)))
	     (let+ (((&values encoded encoded-wire-schema)
		     (domain->wire ,converter domain-object))
		    (decoded (wire->domain ,converter encoded encoded-wire-schema)))
	       (ensure-same domain-object decoded
			    :test (function ,domain-test)))))))))
