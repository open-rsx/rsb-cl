;;; origin-filter.lisp --- Event filtering based on origin id.
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
