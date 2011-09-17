;;; method-filter.lisp --- Event filtering based on method.
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

(defmethod find-filter-class ((spec (eql :method)))
  (find-class 'method-filter))

(defclass method-filter (filter-mixin)
  ((method :initarg  :method
	   :type     (or null string)
	   :reader   filter-method
	   :documentation
	   "Stores the method name to which the filter should restrict
events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :method (missing-required-initarg 'method-filter :method))
  (:documentation
   "This filter discriminates based on the method of events."))

(defmethod matches? ((filter method-filter) (event event))
  (let ((filter-method (filter-method filter))
	(event-method  (event-method event)))
    (case filter-method
      ((nil) (not event-method))
      (t     (and event-method
		  (string= event-method filter-method))))))

(defmethod print-object ((object method-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-method object))))
