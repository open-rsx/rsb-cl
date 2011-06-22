;;; util.lisp --- Unit tests for utilities used in the spread backend.
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

(in-package :rsb.transport.spread.test)

(deftestsuite util-root (transport-spread-root)
  ()
  (:setup
   (clrhash *scope-group-cache*))
  (:documentation
   "Test suite for the utility functions used in the spread
backend."))

(addtest (util-root
          :documentation
	  "Smoke test for the `scope->group/no-cache' function.")
  scope->group/no-cache/smoke

  (ensure-cases (string expected)
      '(("/"         "6666cd76f96956469e7be39d750cc7d")
	("/foo/"     "4f87be8f6e593d167f5fd1ab238cfc2")
	("/foo/bar/" "1c184f3891344400380281315d9e738"))
    (let* ((scope  (make-scope string))
	   (result (scope->group/no-cache scope)))
      (ensure-same
       result
       (concatenate 'string expected '(#\Null))
       :test #'string=))))

(addtest (util-root
          :documentation
	  "Smoke test for the `scope->group' function.")
  scope->group/smoke

  (ensure-cases (scope) '("/foo/bar")
    (let ((result-1 (scope->group (make-scope scope :intern? t)))
	  (result-2 (scope->group (make-scope scope :intern? t)))
	  (result-3 (scope->group (make-scope scope))))
      (ensure-same result-1 result-2 :test #'eq)
      (ensure-same result-1 result-3 :test #'string=))))

(addtest (util-root
          :documentation
	  "Test the cache flushing mechanism used by `scope->group'.")
  scope->group/cache-flush

  (iter (repeat (* 10 *scope-group-cache-max-size*))
	(scope->group (rsb:make-scope "/bla/bli/boo")))

  (ensure (<= (hash-table-count *scope-group-cache*)
	      *scope-group-cache-max-size*)))
