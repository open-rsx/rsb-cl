;;; disjoin-filter.lisp --- Unit test for the disjoin-filter class.
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

(cl:in-package :rsb.filter.test)

(deftestsuite disjoin-filter-root (filter-root
				   filter-suite)
  ((simple-filter (filter
		   '(:or
		     (:scope :scope "/foo/bar")
		     (:type :type string)))))
  (:documentation
   "Unit tests for the `disjoin-filter' class."))

(define-basic-filter-test-cases (disjoin-filter :or)
    ;; Construction
    '(;; these are ok
      (()              t)
      ((:children nil) t))

  ;; Matching results
  t t t nil)
