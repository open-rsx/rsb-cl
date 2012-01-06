;;; util.lisp --- Unit tests for utilities used in the spread backend.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
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

(in-package :rsb.transport.spread.test)

(deftestsuite util-root (transport-spread-root)
  ()
  (:setup
   (clrhash *scope->groups-cache*))
  (:documentation
   "Test suite for the utility functions used in the spread
backend."))

(addtest (util-root
          :documentation
	  "Smoke test for the `scope->group' function.")
  scope->group/smoke

  (ensure-cases (string expected)
      '(("/"         "6666cd76f96956469e7be39d750cc7d")
	("/foo/"     "4f87be8f6e593d167f5fd1ab238cfc2")
	("/foo/bar/" "1c184f3891344400380281315d9e738"))
    (let ((result (scope->group (make-scope string))))
      (ensure-same result (concatenate 'string expected '(#\Null))
		   :test #'string=))))

(addtest (util-root
          :documentation
	  "Smoke test for the non-caching variant of the
`scope->groups' function.")
  scope->groups/no-cache/smoke

  (ensure-cases (scope expected-groups)
      '(("/"        ("6666cd76f96956469e7be39d750cc7d"))
	("/foo"     ("6666cd76f96956469e7be39d750cc7d"
		     "4f87be8f6e593d167f5fd1ab238cfc2"))
	("/foo/bar" ("6666cd76f96956469e7be39d750cc7d"
		     "4f87be8f6e593d167f5fd1ab238cfc2"
		     "1c184f3891344400380281315d9e738")))
    (let ((result   (scope->groups/no-cache (make-scope scope)))
	  (expected (map 'list (lambda (name)
				 (concatenate 'string name '(#\Null)))
			 expected-groups)))
      (ensure-same result expected
		   :test #'(rcurry #'set-equal :test #'string=)))))

(addtest (util-root
          :documentation
	  "Smoke test for the `scope->groups' function.")
  scope->groups/smoke

  (ensure-cases (scope) '("/" "/foo" "/foo/bar")
    (let ((result-1 (scope->groups (make-scope scope :intern? t)))
	  (result-2 (scope->groups (make-scope scope :intern? t)))
	  (result-3 (scope->groups (make-scope scope))))
      (ensure-same result-1 result-2 :test #'eq)
      (ensure-same result-1 result-3 :test #'equal))))

(addtest (util-root
          :documentation
	  "Test the cache flushing mechanism of the cache used by
`scope->groups'.")
  scope->groups/cache-flush

  (iter (repeat (* 10 *scope->groups-cache-max-size*))
	(ensure (<= (hash-table-count *scope->groups-cache*)
		    *scope->groups-cache-max-size*))
	(scope->groups (make-scope "/bla/bli/boo"))
	(ensure (<= (hash-table-count *scope->groups-cache*)
		    *scope->groups-cache-max-size*))))
