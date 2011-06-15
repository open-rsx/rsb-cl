;;; package.lisp --- Package definition filter module.
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

(in-package :cl-user)

(defpackage :rsb.filter
  (:use
   :cl
   :alexandria
   :bind

   :rsb)

  ;; Filter protocol
  (:export
   :matches?)

  ;; Filter class family and construction
  (:export
   :no-such-filter
   :find-filter-class
   :filter-classes

   :make-filter)

  ;; `filter-mixin' class
  (:export
   :filter-mixin)

  (:export
   :composite-filter
   :filter-children)

  ;; `conjoin-filter' class
  (:export
   :conjoin-filter)

  ;; `disjoin-filter' class
  (:export
   :disjoin-filter)

  ;; `fallback-policy-mixin' class
  (:export
   :fallback-policy-mixin
   :filter-fallback-policy)

  ;; `scope-filter' class
  (:export
   :scope-filter
   :filter-scope)

  ;; `origin-filter' class
  (:export
   :origin-filter
   :filter-origin)

  ;; DSL
  (:export
   :filter)

  (:documentation
   "This package contains event filters. In general, filters are unary
predicates that discriminate `rsb:event' instances . The filters in
this package are implemented as funcallable classes with a specialized
`matches?' method as the funcallable instance function."))

(in-package :rsb.filter)

(log5:defcategory rsb.filter)
