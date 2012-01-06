;;; scope-filter.lisp ---
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

(defmethod find-filter-class ((spec (eql :scope)))
  (find-class 'scope-filter))

(defclass scope-filter (filter-mixin
			scope-mixin)
  ((rsb::scope :accessor filter-scope
	       :documentation
	       "A superscope of the scopes of matching events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this filter class discriminate based on the scopes of
events."))

(defmethod matches? ((filter scope-filter) (event event))
  "EVENT is matched by comparing its scope to the scope of FILTER."
  (sub-scope? (event-scope event) (filter-scope filter)))

(defmethod print-object ((object scope-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (scope-string (filter-scope object)))))
