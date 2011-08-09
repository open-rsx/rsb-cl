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

(deftestsuite informer-root (root
			     participant-suite)
  ()
  (:documentation
   "Unit tests for the `informer' class and `make-informer' function."))

(define-basic-participant-test-cases :informer
  '("/informer/construction" (t)                 "/informer/construction")
  '("/"                      (t :transports nil) :error))

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

  (with-informer (informer "/informer/check-type" 'sequence)
    ;; In this case, the event cannot be constructed from the payload.
    (ensure-condition 'type-error
      (send informer 5))
    ;; In this case, the event is not compatible with the informer's
    ;; type.
    (ensure-condition 'invalid-event-type
      (send informer (make-event/typed
		      "/informer/check-type" 5 'integer)))

    ;; The following are compatible.
    (send informer '(1 2))
    (send informer (make-event/typed "/informer/check-type" "bla" 'string))
    (send informer (make-event/typed "/informer/check-type" "bla" 'sequence))))

(addtest (informer-root
          :documentation
	  "Test the scope check employed by the `send' method.")
  check-scope

  (with-informer (informer "/informer/check-scope" t)
    ;; Identical scope and subscopes are allowed
    (send informer (make-event "/informer/check-scope" "foo"))
    (send informer (make-event "/informer/check-scope/subscope" "foo"))

    ;; Scope is not identical to or a subscope of the informer's
    ;; scope.
    (ensure-condition 'invalid-event-scope
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
