;;; scope-filter.lisp ---
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
