;;; reader.lisp --- Unit tests for the reader class.
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

(in-package :rsb.test)

(deftestsuite reader-root (root
			   participant-suite)
  ()
  (:documentation
   "Unit tests for the `reader' class and `make-reader' function."))

(define-basic-participant-test-cases :reader
  '("/reader/construction" nil               "/reader/construction")
  '("/                   " (:transports nil) :error))

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
