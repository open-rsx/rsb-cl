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

  ;; `payload-matching-mixin' class
  (:export
   :payload-matches?
   :payload-matching-mixin)

  ;; `scope-filter' class
  (:export
   :scope-filter
   :filter-scope)

  ;; `type-filter' class
  (:export
   :type-filter
   :filter-type)

  ;; `origin-filter' class
  (:export
   :origin-filter
   :filter-origin)

  ;; `regex-filter' class
  (:export
   :regex-filter
   :filter-regex)

  ;; `xpath-filter' class
  (:export
   :xpath-filter
   :filter-xpath
   :filter-compiled-xpath)

  ;; DSL
  (:export
   :filter)

  (:documentation
   "This package contains event filters. In general, filters are unary
predicates that discriminate arbitrary object and in particular
`rsb:event' instances. The filters in this package are implemented as
funcallable classes with a specialized `matches?' method as the
funcallable instance function."))
