;;; listener.lisp --- Unit tests for listener.
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

(deftestsuite listener-root (root
			     participant-suite)
  ()
  (:function
   (send-some (informer)
     (iter (repeat 100)
	   (send informer "foo")
	   (send informer "bar"))))
  (:documentation
   "Unit tests for the `listener' class and `make-listener'
function."))

(define-basic-participant-test-cases :listener)

(addtest (listener-root
          :documentation
	  "Test participating in a channel via `make-listener'.")
  construction

  (ensure-cases (arg expected-scope)
      '(("/listener/construction"                      "/listener/construction")
	("inprocess://localhost/listener/construction" "/listener/construction"))

    (let ((listener (make-listener arg)))
      (unwind-protect
	   (check-participant listener expected-scope)
	(detach/ignore-errors listener)))))

(addtest (listener-root
          :documentation
	  "Test receiving data sent by an informer.")
  receive

  (with-informer (informer "/foo" t)
    (with-listener (listener "/foo")
      ;; Test receive
      (send-some informer)

      ;; Test receive with filters
      (push (filter :origin :origin (uuid:make-v1-uuid))
	    (receiver-filters listener))
      (send-some informer)

      ;; Test receive with handlers
      (push #'(lambda (event) (declare (ignore event)))
	    (rsb.ep:handlers listener))
      (send-some informer))))
