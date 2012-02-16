;;; type-filter.lisp --- A filter that discriminates event based on their type.
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

(cl:in-package :rsb.filter)

(defmethod find-filter-class ((spec (eql :type)))
  (find-class 'type-filter))

(defclass type-filter (filter-mixin) ;; TODO typed-mixin?
  ((type :initarg  :type
	 :type     (or list symbol)
	 :accessor filter-type
	 :documentation
	 "The type of matching events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this filter class discriminate based on the type of
RSB events."))

(defmethod matches? ((filter type-filter) (event event))
  (typep (event-data event) (filter-type filter)))

(defmethod print-object ((object type-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-type object))))
