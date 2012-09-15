;;; conversion-mixin.lisp --- A mixin for converter selection.
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

(cl:in-package :rsb.transport)

(defclass conversion-mixin ()
  ((converter     :initarg  :converter
		  :accessor connector-converter
		  :documentation
		  "A converter to which the actual conversion work is
delegated.")
   (serialization :accessor connector-serialization
		  :documentation
		  ""))
  (:default-initargs
   :converter (missing-required-initarg
	       'conversion-mixin :converter))
  (:documentation
   "This mixin adds methods on `domain->wire' and `wire->domain' for
the subclass which delegate the conversion tasks to a stored
converter."))

(defmethod shared-initialize :after ((instance   conversion-mixin)
                                     (slot-names t)
                                     &key
				     converter
				     (serialization (%make-default-serialization
						     instance converter)))
  (setf (connector-serialization instance) serialization))



#+no (defmethod domain->wire ((connector     conversion-mixin)
			 (domain-object t))
  "Delegate conversion of DOMAIN-OBJECT to the converter stored in
CONNECTOR."
  (domain->wire (connector-converter connector) domain-object))

#+no (defmethod wire->domain ((connector   conversion-mixin)
			 (wire-data   t)
			 (wire-schema t))
  "Delegate the conversion of WIRE-DATA, WIRE-SCHEMA to the converter
stored in CONNECTOR."
  (wire->domain (connector-converter connector) wire-data wire-schema))


;;;
;;

(defun %make-default-serialization (connector converter)
  "TODO(jmoringe): document"
  (make-instance
   'rsb.serialization::protocol-buffer ;;; TODO(jmoringe, 2012-09-10):
   :converter            converter
   :expose-wire-schema?  (connector-expose?
			  connector :rsb.transport.wire-schema)
   :expose-payload-size? (connector-expose?
			  connector :rsb.transport.payload-size)))
