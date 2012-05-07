;;; package.lisp --- Package definition filter module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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

(cl:defpackage :rsb.filter
  (:use
   :cl
   :alexandria
   :let-plus
   :more-conditions

   :rsb)

  ;; Conditions
  (:export
   :filter-construction-error
   :filter-construction-error-spec)

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
   :filter-regex
   :filter-case-sensitive?

   :compile-regex)

  ;; `method-filter' class
  (:export
   :method-filter
   :filter-method)

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
