;;; protocol.lisp ---
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


;;; Filter protocol
;;

(defgeneric matches? (filter event)
  (:documentation
   "Return non-nil if EVENT matches the criteria of FILTER."))


;;;
;;

(dynamic-classes:define-findable-class-family filter
    "")

(defun make-filter (name &rest args)
  "DOC"
  (apply #'make-instance (find-filter-class name) args))

;; DSL:

(defgeneric filter (spec
		    &rest args
		    &key &allow-other-keys)
  (:documentation
   "SPEC is a list of the form
   (TYPE ARGS)"))

(defmethod filter ((spec symbol)
		   &rest args
		   &key)
  (apply #'make-filter spec args))

(defmethod filter ((spec list)
		   &rest args
		   &key)
  (bind (((class &rest child-specs) spec)
	 (children (mapcar (curry  #'apply #'filter) child-specs)))
    (apply #'filter class :children children args)))

;; TODO compilation protocol?

;; TODO temp here filter mixin

(defclass filter-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "DOC"))

(defmethod initialize-instance :after ((instance filter-mixin)
				       &key)
  (closer-mop:set-funcallable-instance-function
   instance
   #'(lambda (event)
       (matches? instance event))))
