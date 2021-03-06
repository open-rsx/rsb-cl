;;;; informer.lisp --- Unit tests for informer class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite informer-root (root
                             participant-suite)
  ()
  (:documentation
   "Unit tests for the `informer' class."))

(define-basic-participant-test-cases informer
  '("/rsbtest/informer/construction"
    (:type t)
    "/rsbtest/informer/construction")

  '("/rsbtest/informer/construction"
    (:type t :transports ((:inprocess &inherit)))
    "/rsbtest/informer/construction")

  '("/rsbtest/informer/construction"
    (:type t :transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/informer/construction")

  '("/rsbtest/informer/construction"
    (:type t :converters ((t . :foo)))
    "/rsbtest/informer/construction")

  `("/rsbtest/informer/construction"
    (:type t :transform ,#'1+)
    "/rsbtest/informer/construction")

  `("/rsbtest/informer/construction"
    (:type t :parent ,*simple-parent*)
    "/rsbtest/informer/construction")

  '("/rsbtest/informer/construction"
    (:type t :introspection? nil)
    "/rsbtest/informer/construction")

  '("/rsbtest/informer/construction"
    (:type t :introspection? t)
    "/rsbtest/informer/construction")

  '("inprocess:/rsbtest/informer/construction"
    (:type t)
    "/rsbtest/informer/construction")

  `("inprocess:/rsbtest/informer/construction"
    (:type t :error-policy ,#'continue)
    "/rsbtest/informer/construction")

  `("/rsbtest/informer/construction?foo=bar"
    (:type t)
    "/rsbtest/informer/construction")

  ;; No transports => error
  '("/rsbtest/informer/construction"
    (:type t :transports ((t :enabled nil)))
    error))

(addtest (informer-root
          :documentation
          "Test sending data.")
  send/data

  (with-participant (informer :informer "/rsbtest/informer/send")
    (iter (repeat 100)
          (send informer "<foo/>")
          (send informer "<bar/>"))))

(addtest (informer-root
          :documentation
          "Test sending data.")
  send/event

  (with-participant (informer :informer "/rsbtest/informer/send/event")
    (ensure-cases (scope data args
                   expected-meta-data expected-method expected-timestamps
                   expected-causes)
        `(;; Some invalid cases.
          ("/rsbtest/informer/send/event" "foo" (:foo foo) ; invalid meta-data item
           type-error nil nil nil)
          ("/rsbtest/informer/send/event" "foo" (:timestamps (:foo 1)) ; invalid timestamp
           type-error nil nil nil)

          ;; These are valid
          ("/rsbtest/informer/send/event" "foo" ()
           () nil () ())

          ("/rsbtest/informer/send/event" "foo" (:method :bar)
           () :bar () ())

          ("/rsbtest/informer/send/event" "foo" (:foo "baz")
           ((:foo . "baz")) nil () ())
          ("/rsbtest/informer/send/event" "foo" (:foo 1)
           ((:foo . 1)) nil () ())
          ("/rsbtest/informer/send/event" "foo" (:foo :baz)
           ((:foo . :baz)) nil () ())

          ("/rsbtest/informer/send/event" "foo"
           (:timestamps (:foo ,(local-time:parse-timestring
                                "2013-03-27T14:12:32.062533+01:00")))
           () nil ((:foo . ,(local-time:parse-timestring
                             "2013-03-27T14:12:32.062533+01:00"))) ())

          ("/rsbtest/informer/send/event" "foo" (:causes ((,(uuid:make-null-uuid) . 1)))
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
             (ensure-same (remove-if
                           (lambda (name)
                             (member name *framework-timestamps* :test #'eq))
                           (timestamp-alist result) :key #'car)
                          expected-timestamps
                          :test (rcurry #'set-equal :test #'timestamp-entries-equal))
             (ensure-same (event-causes result) expected-causes
                          :test (rcurry #'set-equal :test #'event-id=)))))))))

(addtest (informer-root
          :documentation
          "Test the type check employed by the `send' method.")
  send/check-type

  (with-participant (informer :informer "/rsbtest/informer/send/check-type"
                              :type 'sequence)
    ;; In this case, the event cannot be constructed from the payload.
    (ensure-condition 'type-error
      (send informer 5))
    ;; In this case, the event is not compatible with the informer's
    ;; type.
    (ensure-condition 'event-type-error
      (send informer (make-event "/rsbtest/informer/send/check-type" 5)))

    ;; The following are compatible.
    (send informer '(1 2))
    (send informer (make-event "/rsbtest/informer/send/check-type" "bla"))))

(addtest (informer-root
          :documentation
          "Test the scope check employed by the `send' method.")
  send/check-scope

  (with-participant (informer :informer "/rsbtest/informer/send/check-scope")
    ;; Identical scope and subscopes are allowed
    (send informer (make-event "/rsbtest/informer/send/check-scope" "foo"))
    (send informer (make-event "/rsbtest/informer/send/check-scope/subscope" "foo"))

    ;; Scope is not identical to or a subscope of the informer's
    ;; scope.
    (ensure-condition 'event-scope-error
      (send informer (make-event "/rsbtest/informer/send/wrong-scope" "foo")))))

(addtest (informer-root
          :documentation
          "Test the \"unchecked\" mode of operation of the `send'
           method.")
  send/unchecked

  (with-participant (informer :informer "/rsbtest/informer/send/unchecked"
                              :type 'string)
    ;; Arbitrary scopes should be accepted.
    (send informer (make-event "/rsbtest/informer/send/unchecked" "foo")
          :unchecked? t)
    (send informer (make-event "/rsbtest/informer/send/unchecked/subscope" "foo")
          :unchecked? t)
    (send informer (make-event "/rsbtest/informer/send/wrong-scope" "foo")
          :unchecked? t)

    ;; Arbitrary data types should accepted.
    (send informer (make-event "/rsbtest/informer/send/unchecked" "foo")
          :unchecked? t)
    (send informer (make-event "/rsbtest/informer/send/unchecked" 1)
          :unchecked? t)))

(addtest (informer-root
          :documentation
          "Test the \"unchecked\" mode of operation of the `send'
           method.")
  send/no-fill

  (with-participant (informer :informer "/rsbtest/informer/send/no-fill")
    (let* ((event  (make-event "/rsbtest/informer/send/no-fill" "foo"))
           (event* (send informer event :no-fill? t)))
      (setf (event-origin event)          (uuid:make-null-uuid)
            (event-sequence-number event) 1234)
      (ensure-same (event-origin event*) (event-origin event)
                   :test #'uuid:uuid=)
      (ensure-same (event-sequence-number event*)
                   (event-sequence-number event)))))

(define-error-hook-test-case (informer :participant? nil)
  ;; We cannot currently cause the informer case to fail when using
  ;; inprocess transport. So we just add the error handler without
  ;; exercising it.
  (send informer "foo"))

(addtest (informer-root
          :documentation
          "Test sequence number generator, especially modular
           arithmetic behavior around 2^32.")
  sequence-number-generator

  (ensure-cases (start expected)
      `((0                 (0 1 2))
        (,(- (ash 1 32) 2) (,(- (ash 1 32) 2) ,(- (ash 1 32) 1) 0 1 2)))

    (let ((generator (rsb::make-sequence-number-generator start)))
      (iter (for value/generated next (funcall generator))
            (for value/expected  in   expected)
            (ensure-same value/generated value/expected
                         :test #'=)))))
