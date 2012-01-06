;;; informer.lisp --- Unit tests for informer class.
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

(deftestsuite informer-root (root
			     participant-suite)
  ()
  (:documentation
   "Unit tests for the `informer' class and `make-informer'
function."))

(define-basic-participant-test-cases :informer
  '("/informer/construction"
    (t)
    "/informer/construction")
  '("/informer/construction"
    (t :transports ((:inprocess &inherit)))
    "/informer/construction")
  '("/informer/construction"
    (t :converters ((t . :foo)))
    "/informer/construction")
  '("inprocess:/informer/construction"
    (t)
    "/informer/construction")

  ;; No transports => error
  '("/informer/construction"
    (t :transports nil)
    :error))

(addtest (informer-root
          :documentation
	  "Test sending data.")
  send/data

  (with-informer (informer "/informer/send" t)
    (iter (repeat 100)
	  (send informer "<foo/>")
	  (send informer "<bar/>"))))

(addtest (informer-root
          :documentation
	  "Test sending data.")
  send/event

  (with-informer (informer "/informer/send/event" t)
    (ensure-cases (scope data args expected-method expected-meta-data)
	'(("/informer/send/event" "foo" ()              nil  nil)
	  ("/informer/send/event" "foo" (:method :bar)  :bar nil)
	  ("/informer/send/event" "foo" (:foo    "baz") nil  ((:foo . "baz"))))

      (let* ((event  (make-event scope data))
	     (result (apply #'send informer event args)))
	(ensure-same (event-method result) expected-method
		     :test #'eq)
	(ensure-same (meta-data-alist result) expected-meta-data
		     :test (rcurry #'set-equal :test #'equal))))))

(addtest (informer-root
          :documentation
	  "Test the type check employed by the `send' method.")
  send/check-type

  (with-informer (informer "/informer/send/check-type" 'sequence)
    ;; In this case, the event cannot be constructed from the payload.
    (ensure-condition 'type-error
      (send informer 5))
    ;; In this case, the event is not compatible with the informer's
    ;; type.
    (ensure-condition 'invalid-event-type
      (send informer (make-event/typed
		      "/informer/send/check-type" 5 'integer)))

    ;; The following are compatible.
    (send informer '(1 2))
    (send informer (make-event/typed "/informer/send/check-type" "bla" 'string))
    (send informer (make-event/typed "/informer/send/check-type" "bla" 'sequence))))

(addtest (informer-root
          :documentation
	  "Test the scope check employed by the `send' method.")
  send/check-scope

  (with-informer (informer "/informer/send/check-scope" t)
    ;; Identical scope and subscopes are allowed
    (send informer (make-event "/informer/send/check-scope" "foo"))
    (send informer (make-event "/informer/send/check-scope/subscope" "foo"))

    ;; Scope is not identical to or a subscope of the informer's
    ;; scope.
    (ensure-condition 'invalid-event-scope
      (send informer (make-event "/informer/send/wrong-scope" "foo")))))

(addtest (informer-root
          :documentation
	  "Test the \"unckecked\" mode of operation of the `send'
method.")
  send/unckecked

  (with-informer (informer "/informer/send/unckecked" 'string)
    ;; Arbitrary scopes should be accepted.
    (send informer (make-event "/informer/send/unchecked" "foo")
	  :unchecked? t)
    (send informer (make-event "/informer/send/unchecked/subscope" "foo")
	  :unchecked? t)
    (send informer (make-event "/informer/send/wrong-scope" "foo")
	  :unchecked? t)

    ;; Arbitrary data types should accepted.
    (send informer (make-event/typed "/informer/send/unchecked" "foo" 'string)
	  :unchecked? t)
    (send informer (make-event/typed "/informer/send/unchecked" 1 'integer)
	  :unchecked? t)))
