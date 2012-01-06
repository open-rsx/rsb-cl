;;; reader.lisp --- A converter that uses the Lisp reader/printer.
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

(defmethod wire->domain? ((converter   (eql :reader))
			  (wire-data   string)
			  (wire-schema t))
  (values converter wire-schema))

(defmethod domain->wire? ((converter     (eql :reader))
			  (domain-object t))
  (values converter 'string (type-of domain-object)))

(defmethod wire->domain ((converter   (eql :reader))
			 (wire-data   string)
			 (wire-schema t))
  (with-standard-io-syntax
    (read-from-string wire-data)))

(defmethod wire->domain :around ((converter   (eql :reader))
				 (wire-data   string)
				 (wire-schema t))
  (let ((expected-type wire-schema)
	(result        (call-next-method)))
    (unless (typep result expected-type)
      (error 'wire->domain-conversion-error
	     :wire-schema      wire-schema
	     :encoded          wire-data
	     :domain-type      expected-type
	     :format-control   "~@<The value is not ~A is not of the ~
expected type ~A.~@:>"
	     :format-arguments `(,result ,expected-type)))
    result))

(defmethod domain->wire ((converter     (eql :reader))
			 (domain-object t))
  (values
   (with-standard-io-syntax
     (prin1-to-string domain-object))
   (type-of domain-object)))
