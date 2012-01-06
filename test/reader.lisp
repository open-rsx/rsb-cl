;;; reader.lisp --- Unit tests for the reader class.
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

(cl:in-package :rsb.test)

(deftestsuite reader-root (root
			   participant-suite)
  ()
  (:documentation
   "Unit tests for the `reader' class and `make-reader' function."))

(define-basic-participant-test-cases :reader
  '("/reader/construction"
    nil
    "/reader/construction")
  '("/reader/construction"
    (:transports ((:inprocess &inherit)))
    "/reader/construction")
  '("/reader/construction"
    (:converters ((t . :foo)))
    "/reader/construction")
  '("inprocess:/reader/construction"
    nil
    "/reader/construction")

  ;; No transports => error
  '("/reader/construction"
    (:transports nil)
    :error))

(addtest (reader-root
          :documentation
	  "Test receiving data sent by an informer in a blocking
mode.")
  receive/blocking

  (with-informer (informer "/reader/receive/blocking" t)
    (with-reader (reader "/reader/receive/blocking")
      (ensure-random-cases 32 ((data a-string))
	(send informer data)
	(check-event (receive reader :block? t)
		     "/reader/receive/blocking" data)))))

(addtest (reader-root
          :documentation
	  "Test receiving data sent by an informer in a non-blocking
mode.")
  receive/non-blocking

  (with-informer (informer "/reader/receive/non-blocking" t)
    (with-reader (reader "/reader/receive/non-blocking")
      (ensure-random-cases 32 ((data a-string))
	(send informer data)
	(let ((received
	       (iter (for received next (receive reader :block? nil))
		     (when received
		       (return received)))))
	  (check-event received
		       "/reader/receive/non-blocking" data))))))
