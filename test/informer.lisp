;;;; informer.lisp --- Unit tests for informer class.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(def-suite* informer-root
  :in root
  :description
  "Unit tests for the `informer' class.")

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

(test (send/data :fixture with-configuration)
  "Test sending data."

  (finishes
    (with-participant (informer :informer "/rsbtest/informer/send")
      (iter (repeat 100)
            (send informer "<foo/>")
            (send informer "<bar/>")))))

(test (send/event :fixture with-configuration)
  "Test sending pre-constructed events."

  (with-participant (informer :informer "/rsbtest/informer/send/event")
    (mapc
     (lambda+ ((scope data args
                      expected-meta-data expected-method expected-timestamps
                      expected-causes))
       (let+ (((&flet do-it ()
                 (apply #'send informer (make-event scope data) args)))
              ((&flet+ timestamp-entries-equal ((left-key  . left-timestamp)
                                                (right-key . right-timestamp))
                 (and (eq left-key right-key)
                      (local-time:timestamp= left-timestamp right-timestamp)))))
         (case expected-meta-data
           (type-error
            (signals type-error (do-it)))
           (t
            (let ((result (do-it)))
              (is (set-equal expected-meta-data (meta-data-alist result)
                             :test #'equal))
              (is (eq expected-method (event-method result)))
              (is (set-equal expected-timestamps
                             (remove-if
                              (lambda (name)
                                (member name *framework-timestamps* :test #'eq))
                              (timestamp-alist result) :key #'car)
                             :test #'timestamp-entries-equal))
              (is (set-equal expected-causes (event-causes result)
                             :test #'event-id=)))))))

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
           () nil () ((,(uuid:make-null-uuid) . 1)))))))

(test (send/check-type :fixture with-configuration)
  "Test the type check employed by the `send' method."

  (with-participant (informer :informer "/rsbtest/informer/send/check-type"
                              :type 'sequence)
    ;; In this case, the event cannot be constructed from the payload.
    (signals type-error (send informer 5))
    ;; In this case, the event is not compatible with the informer's
    ;; type.
    (signals event-type-error
      (send informer (make-event "/rsbtest/informer/send/check-type" 5)))

    ;; The following are compatible.
    (finishes (send informer '(1 2)))
    (finishes (send informer (make-event "/rsbtest/informer/send/check-type" "bla")))))

(test (send/check-scope :fixture with-configuration)
  "Test the scope check employed by the `send' method."

  (with-participant (informer :informer "/rsbtest/informer/send/check-scope")
    ;; Identical scope and subscopes are allowed
    (finishes
      (send informer (make-event "/rsbtest/informer/send/check-scope" "foo")))
    (finishes
      (send informer (make-event "/rsbtest/informer/send/check-scope/subscope" "foo")))

    ;; Scope is not identical to or a subscope of the informer's
    ;; scope.
    (signals event-scope-error
      (send informer (make-event "/rsbtest/informer/send/wrong-scope" "foo")))))

(test (send/unchecked :fixture with-configuration)
  "Test the \"unchecked\" mode of operation of the `send' method."

  (with-participant (informer :informer "/rsbtest/informer/send/unchecked"
                              :type 'string)
    ;; Arbitrary scopes should be accepted.
    (finishes
      (send informer (make-event "/rsbtest/informer/send/unchecked" "foo")
            :unchecked? t))
    (finishes
      (send informer (make-event "/rsbtest/informer/send/unchecked/subscope" "foo")
            :unchecked? t))
    (finishes
      (send informer (make-event "/rsbtest/informer/send/wrong-scope" "foo")
            :unchecked? t))

    ;; Arbitrary data types should accepted.
    (finishes
      (send informer (make-event "/rsbtest/informer/send/unchecked" "foo")
            :unchecked? t))
    (finishes
      (send informer (make-event "/rsbtest/informer/send/unchecked" 1)
            :unchecked? t))))

(test (send/no-fill :fixture with-configuration)
  "Test the \"unchecked\" mode of operation of the `send' method."

  (with-participant (informer :informer "/rsbtest/informer/send/no-fill")
    (let* ((event  (make-event "/rsbtest/informer/send/no-fill" "foo"))
           (event* (send informer event :no-fill? t)))
      (setf (event-origin event)          (uuid:make-null-uuid)
            (event-sequence-number event) 1234)
      (is (uuid:uuid= (event-origin event) (event-origin event*)))
      (is (eql (event-sequence-number event*)
               (event-sequence-number event))))))

(define-error-hook-test-case (informer :participant? nil)
  ;; We cannot currently cause the informer case to fail when using
  ;; inprocess transport. So we just add the error handler without
  ;; exercising it.
  (send informer "foo"))

(test sequence-number-generator
  "Test sequence number generator, especially modular arithmetic
   behavior around 2^32."

  (mapc (lambda+ ((start expected))
          (let ((generator (rsb::make-sequence-number-generator start)))
            (iter (for value/generated next (funcall generator))
                  (for value/expected  in   expected)
                  (is (= value/expected value/generated)))))

        `((0                 (0 1 2))
          (,(- (ash 1 32) 2) (,(- (ash 1 32) 2) ,(- (ash 1 32) 1) 0 1 2)))))
