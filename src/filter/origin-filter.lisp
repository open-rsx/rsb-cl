;;; origin-filter.lisp --- Event filtering based on origin id.
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

(in-package :rsb.filter)

(defmethod find-filter-class ((spec (eql :origin)))
  (find-class 'origin-filter))

(defclass origin-filter (filter-mixin)
  ((origin :type     uuid:uuid
	   :accessor filter-origin
	   :documentation
	   ""))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :origin (missing-required-initarg 'origin-filter :origin))
  (:documentation
   "This filter discriminates based on the origin id of events."))

(defmethod shared-initialize :after ((instance   origin-filter)
                                     (slot-names t)
                                     &key
				     origin)
  (setf (slot-value instance 'origin)
	(etypecase origin
	  (uuid:uuid    origin)
	  (string       (uuid:make-uuid-from-string origin))
	  (octet-vector (uuid:byte-array-to-uuid origin)))))

(defmethod matches? ((filter origin-filter) (event event))
  (rsb::uuid= (filter-origin filter) (event-origin event)))

(defmethod print-object ((object origin-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/rsb::print-id/" (filter-origin object))))
