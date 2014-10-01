;;;; local-introspection.lisp --- Unit tests for the local-introspection class.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(deftestsuite local-introspection-root (introspection-root)
  ()
  (:documentation
   "Unit test suite for the `local-introspection' class."))

(rsb.test:define-basic-participant-test-cases (rsb.introspection::local-introspection
                                               :named? nil)
  '("/rsbtest/local-introspection/construction"
    () () ()
    "/rsbtest/local-introspection/construction")

  '("/rsbtest/local-introspection/construction"
    () () (:transports ((:inprocess &inherit)))
    "/rsbtest/local-introspection/construction")

  '("/rsbtest/local-introspection/construction"
    () () (:transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/local-introspection/construction")

  '("/rsbtest/local-introspection/construction"
    () () (:converters ((t . :foo)))
    "/rsbtest/local-introspection/construction")

  '("inprocess:/rsbtest/local-introspection/construction"
    () () ()
    "/rsbtest/local-introspection/construction")

  `("inprocess:/rsbtest/local-introspection/construction"
    () () (:error-policy ,#'continue)
    "/rsbtest/local-introspection/construction")

  `("/rsbtest/local-introspection/construction?foo=bar"
    () () ()
    "/rsbtest/local-introspection/construction")

  ;; No transports => error
  '("/rsbtest/local-introspection/construction"
    () () (:transports ((t :enabled nil)))
    error))

(defun listener-creation-error-caused-by-local-introspection-creation-error? (condition)
  (flet ((type-and-kind? (condition type kind)
           (and (typep condition type)
                (eq (participant-creation-error-kind condition) kind))))
    (and (type-and-kind? condition         'participant-creation-error :listener)
         (type-and-kind? (cause condition) 'participant-creation-error :local-introspection))))

(deftype listener-creation-error-caused-by-local-introspection-creation-error ()
  '(satisfies listener-creation-error-caused-by-local-introspection-creation-error?))

(addtest (local-introspection-root
          :documentation
          "Test conditions signaled when creating a participant fails
           because of local-introspection errors.")
  contruction/chained-participant-creation-error

  ;; Bind `*local-database*' to nil to ensure an attempt is made to
  ;; create the database.
  (let+ ((rsb.introspection::*local-database* nil)
         ((&flet do-it ()
            (ensure-condition 'listener-creation-error-caused-by-local-introspection-creation-error
              (make-participant :listener "/" :transports '((:inprocess :enabled t)
                                                            (t :enabled nil)))))))
    ;; `local-introspection' creation fails because configuration does
    ;; not select a transport.
    (let ((*configuration* '(((:transport :socket :enabled) . nil)
                             ((:introspection :enabled)     . t))))
      (do-it))
    ;; `local-introspection' creation fails because configuration
    ;; specifies a non-existent transport.
    (let ((*configuration* '(((:transport :socket :enabled)            . nil)
                             ((:transport :no-such-transport :enabled) . t)
                             ((:introspection :enabled)                . t))))
      (do-it))))

(addtest (local-introspection-root
          :documentation
          "Smoke test for the `local-introspection' class.")
  smoke

  (with-participant (introspection
                     (make-participant
                      :local-introspection +introspection-scope+))
    ;; Survey existing participants.
    (send-introspection-event rsb.converter:+no-value+
                              :scope-suffix "/participants"
                              :method       :|survey|)

    ;; Call "echo" method of introspection server.
    (let ((server-scope (participant-scope
                         (rsb.introspection::introspection-%server
                          introspection))))
      (with-remote-server (server server-scope)
        (call server "echo" rsb.converter:+no-value+)))))

(addtest (local-introspection-root
          :documentation
          "Test application of the configured error policy in
           `local-introspection' instances.")
  error-policy

  (with-condition-tracking (record-and-continue check-conditions)
    (with-participant (introspection
                       (make-participant
                        :local-introspection +introspection-scope+
                        :error-policy        #'record-and-continue))
      ;; All of the following are ignored because of the survey
      ;; filter.
      (send-introspection-event :foo)
      (send-introspection-event "ping")
      (send-introspection-event rsb.converter:+no-value+)
      (check-conditions '())

      ;; Invalid request events.
      (send-introspection-event "ping" :method :|request|)
      (send-introspection-event rsb.converter:+no-value+ :method :|request|)
      (check-conditions
       '((introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")
         (introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")))

      (send-introspection-event "ping" :method :|request| :suffix-scope "foo")
      (send-introspection-event
       rsb.converter:+no-value+ :method :|request| :suffix-scope "foo")
      (check-conditions
       '((introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")
         (introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")))

      ;; Invalid survey events.
      (send-introspection-event
       "ping"
       :method       :|survey|
       :suffix-scope (princ-to-string (uuid:make-null-uuid)))
      (send-introspection-event
       rsb.converter:+no-value+
       :method       :|survey|
       :suffix-scope (princ-to-string (uuid:make-null-uuid)))
      (check-conditions
       '((introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")
         (introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")))

      (send-introspection-event "ping" :method :|survey| :suffix-scope "foo")
      (send-introspection-event
       rsb.converter:+no-value+ :method :|survey| :suffix-scope "foo")
      (check-conditions
       '((introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")
         (introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol"))))))

(addtest (local-introspection-root
          :documentation
          "Stress test which surveys a `local-introspection' from
           multiple threads in parallel while creating and destroying
           participants in a second set of threads.")
  stress

  (with-participant (introspection
                     (make-participant
                      :local-introspection +introspection-scope+))
    (let+ ((configuration *configuration*)
           ((&flet participant-noise ()
              (let ((*local-database* introspection))
                (iter (repeat 30)
                      (with-local-server (server (string (gensym "/")))
                        (with-methods (server)
                          (("echo" (x) x) ("echo2" (x) x) ("echo3" (x) x))))))))
           ((&flet survey-noise ()
              (with-informer (i (introspection-participants-scope) t)
                (iter (repeat 30)
                      (send i rsb.converter:+no-value+
                            :method :|survey|)))))
           ((&flet make-thread-thunk (i)
              (lambda ()
                (let ((*configuration* configuration))
                  (if (oddp i)
                      (participant-noise)
                      (survey-noise))))))
           (threads (mapcar (compose #'bt:make-thread #'make-thread-thunk)
                            (iota 10))))
      (mapc #'bt:join-thread threads))))
