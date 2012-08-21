;;; notification-filtering-receiver-mixin.lisp --- Unit tests for the notification-filtering-receiver-mixin class.
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

(cl:in-package :rsb.transport.filter.test)

(defclass notification-filtering-mock-connector (notification-filtering-receiver-mixin)
  ((notifications :initarg  :notifications
		  :type     list
		  :accessor connector-notifications)))

(defmethod receive-message ((connector notification-filtering-mock-connector)
			    (block?    t))
  (pop (connector-notifications connector)))

(deftestsuite notification-filtering-receiver-mixin-root (transport-filter-root)
  ((simple-connector))
  (:setup
   (setf simple-connector
	 (make-instance 'notification-filtering-mock-connector
			:notifications '(:a nil nil :b :c :b :a nil)))
   (push (rcurry #'member '(:a :c))
	 (connector-notification-filters simple-connector)))
  (:run-setup :once-per-test-case)
  (:documentation
   "Unit test suite for the
`notification-filtering-receiver-connector' class."))

(addtest (notification-filtering-receiver-mixin-root
          :documentation
	  "Test receiving a filtered notification stream in blocking
  receive mode.")
  smoke/blocking

  (let ((result (iter (repeat 3)
		      (collect (receive-message simple-connector t)))))
    (ensure-same result '(:a :c :a)
		 :test #'equal)))

(addtest (notification-filtering-receiver-mixin-root
          :documentation
	  "Test receiving a filtered notification stream in
non-blocking receive mode.")
  smoke/non-blocking

  (let ((result (iter (repeat 8)
		      (collect (receive-message simple-connector nil)))))
    (ensure-same result '(:a nil nil nil :c nil :a nil)
		 :test #'equal)))
