;;; informer.lisp --- Unit tests for informer class.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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
  `("/informer/construction"
    (t :transform ,#'1+)
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
    (ensure-cases (scope data args
		   expected-meta-data expected-method expected-timestamps
		   expected-causes)
	`(;; Some invalid cases.
	  ("/informer/send/event" "foo" (:foo foo) ; invalid meta-data item
	   type-error nil nil nil)
	  ("/informer/send/event" "foo" (:timestamps (:foo 1)) ; invalid timestamp
	   type-error nil nil nil)

	  ;; These are valid
	  ("/informer/send/event" "foo" ()
	   () nil () ())

	  ("/informer/send/event" "foo" (:method :bar)
	   () :bar () ())

	  ("/informer/send/event" "foo" (:foo "baz")
	   ((:foo . "baz")) nil () ())
	  ("/informer/send/event" "foo" (:foo 1)
	   ((:foo . 1)) nil () ())
	  ("/informer/send/event" "foo" (:foo :baz)
	   ((:foo . :baz)) nil () ())

	  ("/informer/send/event" "foo"
	   (:timestamps (:foo ,(local-time:parse-timestring
				"2013-03-27T14:12:32.062533+01:00")))
	   () nil ((:foo . ,(local-time:parse-timestring
			     "2013-03-27T14:12:32.062533+01:00"))) ())

	  ("/informer/send/event" "foo" (:causes ((,(uuid:make-null-uuid) . 1)))
	   () nil () ((,(uuid:make-null-uuid) . 1))))

      (let+ (((&flet do-it ()
		(apply #'send informer (make-event scope data) args)))
	     ((&flet+ timestamp-entries-equal ((left-key  . left-timestamp)
					       (right-key . right-timestamp))
		(and (eq left-key right-key)
		     (local-time:timestamp= left-timestamp right-timestamp)))))
	(case expected-meta-data
	  (type-error
	   (ensure-condition 'type-error (do-it)))
	  (t
	   (let ((result (do-it)))
	     (ensure-same (meta-data-alist result) expected-meta-data
			  :test (rcurry #'set-equal :test #'equal))
	     (ensure-same (event-method result) expected-method
			  :test #'eq)
	     (ensure-same (remove :create (timestamp-alist result) :key #'car)
			  expected-timestamps
			  :test (rcurry #'set-equal :test #'timestamp-entries-equal))
	     (ensure-same (event-causes result) expected-causes
			  :test (rcurry #'set-equal :test #'event-id=)))))))))

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
      (send informer (make-event "/informer/send/check-type" 5)))

    ;; The following are compatible.
    (send informer '(1 2))
    (send informer (make-event "/informer/send/check-type" "bla"))))

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
    (send informer (make-event "/informer/send/unchecked" "foo")
	  :unchecked? t)
    (send informer (make-event "/informer/send/unchecked" 1)
	  :unchecked? t)))

(define-error-hook-test-case (informer :participant? nil)
  ;; We cannot currently cause the informer case to fail when using
  ;; inprocess transport. So we just add the error handler without
  ;; exercising it.
  (send informer "foo"))

(addtest (informer-root
	  :documentation
	  "Test sequence number generator, especially modular
arithmetic behavior around 1**32.")
  sequence-number-generator

  (ensure-cases (start expected)
      `((0                 (0 1 2))
	(,(- (ash 1 32) 2) (,(- (ash 1 32) 2) ,(- (ash 1 32) 1) 0 1 2)))

    (let ((generator (rsb::make-sequence-number-generator start)))
      (iter (for value/generated next (funcall generator))
	    (for value/expected  in   expected)
	    (ensure-same value/generated value/expected
			 :test #'=)))))
