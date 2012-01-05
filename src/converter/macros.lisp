;;; macros.lisp --- Macros related to defining converters.
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

(defmacro define-simple-converter ((name wire-schema data-type
				    &key
				    (wire-type       'octet-vector)
				    (wire-type-class 'simple-array)
				    (data-type-class data-type      data-type-class-supplied?))
				   (&body wire->domain)
				   (&body domain->wire))
  "Define a converter named NAME that acts on the triple
\(WIRE-TYPE WIRE-SCHEMA DATA-TYPE) as specified by the forms
WIRE->DOMAIN and DOMAIN->WIRE.

The WIRE->DOMAIN form receives the wire-data and wire-schema in
variables called WIRE-DATA and WIRE-SCHEMA. The form should return a
single value which is the deserialized domain object.

The DOMAIN->WIRE form receives the domain object in a variable called
DOMAIN-OBJECT. The form has to return a single value which is the
serialized representation of the domain object."
  (let ((specializer (typecase wire-schema
		       (keyword `(eql ,wire-schema))
		       (t        wire-schema))))
    `(progn
       (defmethod wire->domain? ((converter   (eql ,name))
				 (wire-data   ,wire-type-class)
				 (wire-schema ,specializer))
	 (when (typep wire-data ',wire-type)
	   (values converter ',data-type)))

       (defmethod domain->wire? ((converter     (eql ,name))
				 (domain-object ,data-type-class))
	 ,(if data-type-class-supplied?
	      `(when (typep domain-object ',data-type)
		 (values converter 'octet-vector ,wire-schema))
	      `(values converter 'octet-vector ,wire-schema)))

       (defmethod wire->domain ((converter   (eql ,name))
				(wire-data   ,wire-type-class)
				(wire-schema ,specializer))
	 (check-type wire-data ,wire-type)

	 ,@wire->domain)

       (defmethod domain->wire ((converter     (eql ,name))
				(domain-object ,data-type-class))
	 (check-type domain-object ,data-type)

	 (values (progn ,@domain->wire) ,wire-schema)))))
