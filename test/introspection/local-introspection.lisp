;;;; local-introspection.lisp --- Unit tests for the local-introspection class.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(def-suite* local-introspection-root
  :in introspection-root
  :description
  "Unit test suite for the `local-introspection' class.")

(rsb.test:define-basic-participant-test-cases (rsb.introspection::local-introspection)
  '("/rsbtest/local-introspection/construction"
    ()
    "/rsbtest/local-introspection/construction")

  '("/rsbtest/local-introspection/construction"
    (:transports ((:inprocess &inherit)))
    "/rsbtest/local-introspection/construction")

  '("/rsbtest/local-introspection/construction"
    (:transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/local-introspection/construction")

  '("/rsbtest/local-introspection/construction"
    (:converters ((t . :foo)))
    "/rsbtest/local-introspection/construction")

  '("inprocess:/rsbtest/local-introspection/construction"
    ()
    "/rsbtest/local-introspection/construction")

  `("inprocess:/rsbtest/local-introspection/construction"
    (:error-policy ,#'continue)
    "/rsbtest/local-introspection/construction")

  `("/rsbtest/local-introspection/construction?foo=bar"
    ()
    "/rsbtest/local-introspection/construction")

  ;; No transports => error
  '("/rsbtest/local-introspection/construction"
    (:transports ((t :enabled nil)))
    error))

(defun listener-creation-error-caused-by-local-introspection-creation-error? (condition)
  (flet ((type-and-kind? (condition type kind)
           (and (typep condition type)
                (eq (participant-creation-error-kind condition) kind))))
    (and (type-and-kind? condition         'participant-creation-error :listener)
         (type-and-kind? (cause condition) 'participant-creation-error :local-introspection))))

(deftype listener-creation-error-caused-by-local-introspection-creation-error ()
  '(satisfies listener-creation-error-caused-by-local-introspection-creation-error?))

(test contruction/chained-participant-creation-error
  "Test conditions signaled when creating a participant fails because
   of local-introspection errors."

  ;; Bind `*local-database*' to nil to ensure an attempt is made to
  ;; create the database.
  (let+ ((rsb.introspection::*local-database* nil)
         ((&flet do-it ()
            (signals listener-creation-error-caused-by-local-introspection-creation-error
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

(test (local-introspection/smoke
       :fixture (with-configuration +introspection-configuration+))
  "Smoke test for the `local-introspection' class."

  (with-participant (introspection :local-introspection +introspection-scope+)
    ;; Survey existing participants.
    (send-introspection-event rsb.converter:+no-value+
                              :scope-suffix "/participants"
                              :method       :|survey|)

    ;; Call "echo" method of introspection server.
    (let ((server-scope (participant-scope
                         (rsb.patterns:participant-child
                          introspection :server :local-server))))
      (with-participant (server :remote-server server-scope)
        (finishes (call server "echo" rsb.converter:+no-value+))))))

(test (local-introspection/error-policy
       :fixture (with-configuration +introspection-configuration+))
  "Test application of the configured error policy in
   `local-introspection' instances."

  (with-condition-tracking (record-and-continue check-conditions)
    (with-participant (nil :local-introspection +introspection-scope+
                           :error-policy        #'record-and-continue)
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

      (send-introspection-event "ping" :method :|request| :suffix-scope "/foo")
      (send-introspection-event
       rsb.converter:+no-value+ :method :|request| :suffix-scope "/foo")
      (check-conditions
       '((introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")
         (introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")))

      ;; Invalid survey events.
      (send-introspection-event
       "ping"
       :method       :|survey|
       :suffix-scope (format nil "/~A" (uuid:make-null-uuid)))
      (send-introspection-event
       rsb.converter:+no-value+
       :method       :|survey|
       :suffix-scope (format nil "/~A" (uuid:make-null-uuid)))
      (check-conditions
       '((introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")
         (introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")))

      (send-introspection-event "ping" :method :|survey| :suffix-scope "/foo")
      (send-introspection-event
       rsb.converter:+no-value+ :method :|survey| :suffix-scope "/foo")
      (check-conditions
       '((introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol")
         (introspection-protocol-error
          "not conform to the REQUEST role within the INTROSPECTION protocol"))))))

(test (local-introspection/stress
       :fixture (with-configuration +introspection-configuration+))
  "Stress test which surveys a `local-introspection' from multiple
   threads in parallel while creating and destroying participants in a
   second set of threads."

  (with-participant (introspection :local-introspection +introspection-scope+)
    (let+ ((configuration *configuration*)
           ((&flet participant-noise ()
              (let ((*local-database* introspection))
                (iter (repeat 30)
                      (with-participant (server :local-server (string (gensym "/")))
                        (with-methods (server)
                          (("echo" (x) x) ("echo2" (x) x) ("echo3" (x) x))))))))
           ((&flet survey-noise ()
              (with-participant (i :informer (introspection-participants-scope))
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
