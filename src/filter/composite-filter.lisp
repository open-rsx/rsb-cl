;;; composite-filter.lisp ---
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(in-package :rsb.filter)

(defclass composite-filter (filter-mixin)
  ((children :initarg  :children
	     :type     list
	     :accessor filter-children
	     :initform nil
	     :documentation
	     ""))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of subclasses of this class implement complex filtering
behavior by combining decisions of a set of subordinate filters."))

(defmethod print-object ((object composite-filter) stream)
  (with-slots (children) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(~D)" (length children)))))


;;; Class `conjoin-filter'
;;

(defmethod find-filter-class ((spec (eql :and)))
  (find-class 'conjoin-filter))

(defclass conjoin-filter (composite-filter)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class make filtering decisions by forming the
logical and of the decisions made by their subordinate filters."))

(defmethod matches? ((filter conjoin-filter) (event event))
  (every (rcurry #'matches? event) (filter-children filter)))


;;; Class `disjoin-filter'
;;

(defmethod find-filter-class ((spec (eql :or)))
  (find-class 'disjoin-filter))

(defclass disjoin-filter (composite-filter)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class make filtering decisions by forming the
logical or of the decisions made by their subordinate filters."))

(defmethod matches? ((filter disjoin-filter) (event event))
  (some (rcurry #'matches? event) (filter-children filter)))
