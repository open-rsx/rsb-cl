;;; method-filter.lisp --- Unit tests for the method-filter class.
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

(deftestsuite method-filter-root (filter-root
				  filter-suite)
  ((simple-filter (make-instance 'method-filter
				 :method "FOO")))
  (:documentation
   "Test suite for the `method-filter' class."))

(define-basic-filter-test-cases (method-filter :method)
    ;; Construct cases
    '(;; missing :method initarg
      (()                 :error)

      ;; these are ok
      ((:method nil)      t)
      ((:method "method") t))

  ;; Expected matching results
  nil nil nil nil nil nil nil)
