;;; disjoin-filter.lisp --- Unit test for the disjoin-filter class.
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

(in-package :rsb.filter.test)

(deftestsuite disjoin-filter-root (filter-root
				   filter-suite)
  ((simple-filter (rsb.filter::filter
		   '(:or
		     (:scope :scope "/foo/bar")
		     (:type :type string)))))
  (:documentation
   "Unit tests for the `disjoin-filter' class."))

(define-basic-filter-test-cases (disjoin-filter :or)
    ;; Construction
    '(;; these are ok
      (()              :on)
      ((:children nil) :ok))
  ;; Matching results
  nil t t t t t nil)
