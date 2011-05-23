;;; type-filter.lisp --- A filter that discriminates event based on their type.
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
  (nth-value 0 (subtypep (event-type event) (filter-type filter))))

(defmethod print-object ((object type-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-type object))))
