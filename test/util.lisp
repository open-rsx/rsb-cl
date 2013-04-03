;;; util.lisp --- Unit tests for utility classes and functions.
;;
;; Copyright (C) 2013 Jan Moringen
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

(cl:in-package :rsb.test)

(deftestsuite uuid-mixin-root (root)
  ()
  (:documentation
   "Test suite for the `uuid-mixin' class."))

(addtest (uuid-mixin-root
          :documentation
	  "Test constructing `uuid-mixin' instances for different
values of `*id-random-state*'.")
  random-state

  ;; Binding `*id-random-state*' to identical values has to result in
  ;; `uuid-mixin' instances with identical ids.
  (let+ ((state (make-random-state))
	 ((&values a b)
	  (values
	   (let ((*id-random-state* (make-random-state state)))
	     (make-instance 'uuid-mixin))
	   (let ((*id-random-state* (make-random-state state)))
	     (make-instance 'uuid-mixin)))))
    (ensure-same (slot-value a 'rsb::id) (slot-value b 'rsb::id)
		 :test #'uuid:uuid=))

  ;; Using the current value of `*id-random-state*' multiple times has
  ;; to result in different ids.
  (ensure-different
   (slot-value (make-instance 'uuid-mixin) 'rsb::id)
   (slot-value (make-instance 'uuid-mixin) 'rsb::id)
   :test #'uuid:uuid=))
