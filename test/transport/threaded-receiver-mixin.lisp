;;; threaded-receiver-mixin.lisp --- Unit tests for the threaded-receive-mixin class.
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

(in-package :rsb.transport.test)

(defclass mock-receiver (threaded-receiver-mixin)
  ())

(defmethod receive-messages ((receiver mock-receiver))
  "This causes a timeout of the test case, if the interruption does
not work properly."
  (iter (sleep 100)))

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
	  (notify receiver (make-scope "/") :attached)
	  (notify receiver (make-scope "/") :detached)

	  (notify receiver (make-scope "/") :attached)
	  (sleep .01)
	  (notify receiver (make-scope "/") :detached))))
