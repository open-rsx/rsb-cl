;;; conditions.lisp --- Conditions used in the filter module of cl-rsb.
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

(define-condition filter-construction-error (rsb-error
					     chainable-condition)
  ((spec :initarg  :spec
	 :type     list
	 :reader   filter-construction-error-spec
	 :documentation
	 "The filter specification for which the attempt to construct
a filter instance failed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to construct filter based on ~
specification ~S~/rsb::maybe-print-cause/~@:>"
	     (filter-construction-error-spec condition)
	     (rsb:chainable-condition-cause  condition))))
  (:documentation
   "This error is signaled when an attempt to construct a filter
instance based on a filter specification fails."))
