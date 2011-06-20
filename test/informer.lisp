;;; informer.lisp --- Unit tests for informer class.
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

(deftestsuite informer-root (root)
  ()
  (:documentation
   "Unit tests for the `informer' class and `make-informer' function."))

(define-basic-participant-test-cases :informer)

(addtest (informer-root
          :documentation
	  "Test creating a informer.")
  construction

  (let ((informer (make-informer "/informer/construction" t)))
    (unwind-protect
	 (check-participant informer)
      (detach/ignore-errors informer))))

(addtest (informer-root
          :documentation
	  "Test sending data.")
  send

  (with-informer (informer "/informer/send" t)
    (iter (repeat 100)
	  (send informer "<foo/>")
	  (send informer "<bar/>"))))

(addtest (informer-root
          :documentation
	  "Test the type check employed by the `send' method.")
  check-type

  (with-informer (informer "/informer/check-type" 'string)
    (ensure-condition 'type-error
      (send informer 5))
    (ensure-condition 'invalid-event-type
      (send informer (make-event/typed
		      "/informer/check-type" 5 'integer)))))

(addtest (informer-root
          :documentation
	  "Test the scope check employed by the `send' method.")
  check-scope

  (with-informer (informer "/informer/check-scope" 'string)
    (ensure-condition 'invalid-event-type
      (send informer (make-event "/informer/wrong-scope" "foo")))))

;; (addtest (informer-root
;;           :documentation
;;	  "Test binding *informer-stream*.")
;;   informer-stream
;;
;;   (with-informer (pub "stream")
;;     (ensure-same
;;      (with-output-to-string (stream)
;;        (let ((*informer-stream* stream))
;;	 (send pub "<bla/>")))
;;      "<bla/>"
;;      :test #'string=)))
