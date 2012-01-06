;;; composite-filter.lisp ---
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

(in-package :rsb.filter)

(defclass composite-filter (filter-mixin)
  ((children :initarg  :children
	     :type     list
	     :accessor filter-children
	     :initform nil
	     :documentation
	     "A list of subordinate filters."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of subclasses of this class implement complex filtering
behavior by combining decisions of a set of subordinate filters. On
rare occasions is it useful to make instances of this class itself
rather than subclasses."))

(defmethod print-object ((object composite-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (filter-children object)))))


;;; Class `conjoin-filter'
;;

(defmethod find-filter-class ((spec (eql :conjoin)))
  (find-class 'conjoin-filter))

(defmethod find-filter-class ((spec (eql :and)))
  (find-class 'conjoin-filter))

(defclass conjoin-filter (composite-filter)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class make filtering decisions by forming the
logical and of the decisions made by their subordinate filters."))

(defmethod matches? ((filter conjoin-filter) (event t))
  (every (rcurry #'matches? event) (filter-children filter)))


;;; Class `disjoin-filter'
;;

(defmethod find-filter-class ((spec (eql :disjoin)))
  (find-class 'disjoin-filter))

(defmethod find-filter-class ((spec (eql :or)))
  (find-class 'disjoin-filter))

(defclass disjoin-filter (composite-filter)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class make filtering decisions by forming the
logical or of the decisions made by their subordinate filters."))

(defmethod matches? ((filter disjoin-filter) (event t))
  (some (rcurry #'matches? event) (filter-children filter)))
