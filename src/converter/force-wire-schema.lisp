;;; force-wire-schema.lisp --- A converter that sets a given wire-schema.
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

(in-package :rsb.converter)

(defmethod find-converter-class ((spec (eql :force-wire-schema)))
  (find-class 'force-wire-schema))

(defclass force-wire-schema ()
  ((wire-schema :initarg  :wire-schema
		:type     keyword
		:accessor converter-wire-schema
		:initform :bytes
		:documentation
		"Stores the wire-schema that should be used when
performing domain->wire \"conversions\"."))
  (:documentation
   "Instances of this class do not perform any changes when converting
between wire-data and domain-data but set a given wire-schema when
producing wire-data, wire-schema pairs."))

(defmethod wire->domain? ((converter   force-wire-schema)
			  (wire-data   t)
			  (wire-schema t))
  "The converter can handle arbitrary wire-data."
  (values converter t))

(defmethod domain->wire? ((converter     force-wire-schema)
			  (domain-object t))
  "The converter can handle arbitrary domain objects."
  (bind (((:accessors-r/o
	   (wire-schema converter-wire-schema)) converter))
    (values converter t wire-schema)))

(defmethod wire->domain ((converter   force-wire-schema)
			 (wire-data   t)
			 (wire-schema t))
  "The wire-data is not modified."
  wire-data)

(defmethod domain->wire ((converter     force-wire-schema)
			 (domain-object t))
  "The domain object is not modified, but the configured wire-schema
is set."
  (bind (((:accessors-r/o
	   (wire-schema converter-wire-schema)) converter))
    (values domain-object wire-schema)))

(defmethod print-object ((object force-wire-schema) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (converter-wire-schema object))))
