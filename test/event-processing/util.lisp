;;; util.lisp --- Unit tests for utility functions of the event-processing module.
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

(cl:in-package :rsb.event-processing.test)

(deftestsuite util-root (event-processing-root)
  ()
  (:documentation
   "Unit tests for the utility functions of the event-processing module."))

(addtest (util-root
          :documentation
	  "Smoke test for the `merge-implementation-info' function.")
  merge-implementation-infos-smoke

  (ensure-cases (input expected)
      '((()                              :implemented)
	((:not-implemented)              :not-implemented)
	((:implemented)                  :implemented)
	((:implemented :not-implemented) :not-implemented)
	((:implemented :implemented)     :implemented))

    (let ((result (reduce #'merge-implementation-infos input)))
      (ensure-same result expected
		   :test #'eq))))
