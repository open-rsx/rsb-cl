;;; threaded-receiver-mixin.lisp --- Unit tests for the threaded-receive-mixin class.
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

(in-package :rsb.transport.test)

(defclass mock-receiver (threaded-receiver-mixin)
  ())

(defmethod receive-messages ((receiver mock-receiver))
  "This causes a timeout of the test case, if the interruption does
not work properly."
  (sleep most-positive-fixnum))

(deftestsuite threaded-receiver-mixin-root (transport-root)
  ()
  (:documentation
   "Unit tests for the `threaded-receiver-mixin' class."))

(addtest (threaded-receiver-mixin-root
          :documentation
	  "Smoke test for the `threaded-receiver-mixin' class.")
  smoke

  ;; As a workaround for https://bugs.launchpad.net/asdf/+bug/507378,
  ;; force `receive-messages' to be updated.
  #+sbcl (handler-case
	     (sb-ext:with-timeout .1
	       (receive-messages (make-instance 'mock-receiver)))
	   (sb-ext:timeout (condition) (declare (ignore condition))))

  ;; We try attaching and detaching with different timing behaviors.
  (let ((receiver (make-instance 'mock-receiver)))
    (iter (repeat 100)
	  (start-receiver receiver)
	  (stop-receiver receiver)

	  (start-receiver receiver)
	  (sleep .001)
	  (stop-receiver receiver))))
