;;;; remote-introspection.lisp --- Unit tests for the remote-introspection class.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

;;; Class `introspection-receiver'

(deftestsuite introspection-receiver-root ()
  ()
  (:documentation
   "Unit test suite for the `introspection-receiver' class."))

(define-basic-participant-test-cases (rsb.introspection::introspection-receiver
                                      :named? nil)
  '("/rsbtest/introspection-receiver/construction"
    () () ()
    "/rsbtest/introspection-receiver/construction")

  '("/rsbtest/introspection-receiver/construction"
    () () (:transports ((:inprocess &inherit)))
    "/rsbtest/introspection-receiver/construction")

  '("/rsbtest/introspection-receiver/construction"
    () () (:transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/introspection-receiver/construction")

  '("/rsbtest/introspection-receiver/construction"
    () () (:converters ((t . :foo)))
    "/rsbtest/introspection-receiver/construction")

  '("inprocess:/rsbtest/introspection-receiver/construction"
    () () ()
    "/rsbtest/introspection-receiver/construction")

  `("inprocess:/rsbtest/introspection-receiver/construction"
    () () (:error-policy ,#'continue)
    "/rsbtest/introspection-receiver/construction")

  `("/rsbtest/introspection-receiver/construction?foo=bar"
    () () ()
    "/rsbtest/introspection-receiver/construction")

  ;; No transports => error
  '("/rsbtest/introspection-receiver/construction"
    () () (:transports ((t :enabled nil)))
    error))

;;; Class `remote-introspection-database'

(deftestsuite remote-introspection-database-root (introspection-root)
  ()
  (:documentation
   "Unit test suite for the `remote-introspection-database' class."))

;;; TODO test construction
(addtest (remote-introspection-database-root
          :documentation
          "TODO")
  construction

  )

(addtest (remote-introspection-database-root
          :documentation
          "TODO")
  print

  (let ((introspection (make-instance 'remote-introspection-database)))
    (ensure (not (emptyp (princ-to-string introspection))))))

(addtest (remote-introspection-database-root
          :documentation
          "TODO")
  smoke

  (let* ((ri (make-instance 'remote-introspection-database))
         (id (uuid:make-v4-uuid)))
    (rsb.ep:handle ri (make-instance 'hello
                                     :participant (make-instance 'remote-participant-info
                                                                 :kind :reader
                                                                 :id id
                                                                 :scope (make-scope "/bla")
                                                                 :type t
                                                                 :transports '())
                                     :process (make-instance 'remote-process-info :process-id 8587 :program-name "bla" :transports '())
                                     :host (make-instance 'remote-host-info :id "1234" :hostname "ferberit")))
    (rsb.ep:handle ri (make-instance 'hello
                                     :participant (make-instance 'remote-participant-info
                                                                 :kind :reader
                                                                 :id (uuid:make-v4-uuid)
                                                                 :scope (make-scope "/bla")
                                                                 :type t
                                                                 :transports '())
                                     :process (make-instance 'remote-process-info :process-id 0 :program-name "bla" :transports '())
                                     :host (make-instance 'remote-host-info :id "5678" :hostname "bla")))
    (rsb.ep:handle ri (make-instance 'bye :id id))
    ri))

;;; Class `remote-introspection'

(deftestsuite remote-introspection-root (introspection-root)
  ()
  (:documentation
   "Unit test suite for the `remote-introspection' class."))

(define-basic-participant-test-cases (rsb.introspection::remote-introspection
                                      :named? nil)
  '("/rsbtest/remote-introspection/construction"
    () () ()
    "/rsbtest/remote-introspection/construction")

  '("/rsbtest/remote-introspection/construction"
    () () (:transports ((:inprocess &inherit)))
    "/rsbtest/remote-introspection/construction")

  '("/rsbtest/remote-introspection/construction"
    () () (:transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/remote-introspection/construction")

  '("/rsbtest/remote-introspection/construction"
    () () (:converters ((t . :foo)))
    "/rsbtest/remote-introspection/construction")

  '("/rsbtest/remote-introspection/construction"
    () () (:receiver-uris ("inprocess:"))
    "/rsbtest/remote-introspection/construction")

  '("/rsbtest/remote-introspection/construction"
    () () (:response-timeout 2.0)
    "/rsbtest/remote-introspection/construction")

  '("inprocess:/rsbtest/remote-introspection/construction"
    () () ()
    "/rsbtest/remote-introspection/construction")

  `("inprocess:/rsbtest/remote-introspection/construction"
    () () (:error-policy ,#'continue)
    "/rsbtest/remote-introspection/construction")

  `("/rsbtest/remote-introspection/construction?foo=bar"
    () () ()
    "/rsbtest/remote-introspection/construction")

  ;; No transports => error
  '("/rsbtest/remote-introspection/construction"
    () () (:transports ((t :enabled nil)))
    error))

(addtest (remote-introspection-root
          :documentation
          "Smoke test for timer-based updating of
           `remote-introspection'.")
  update/smoke

  (with-participant (introspection (make-participant
                                    :remote-introspection +introspection-scope+
                                    :update-interval .2))
    (sleep 1.0)))

(addtest (remote-introspection-root
          :documentation
          "Test application of the configured error policy in
           `remote-introspection' instances.")
  error-policy

  (with-condition-tracking (record-and-continue check-conditions)
    (with-participant (introspection (make-participant
                                      :remote-introspection +introspection-scope+
                                      :error-policy #'record-and-continue))
      ;; Invalid scopes
      (send-introspection-event "pong")
      (check-conditions
       '((introspection-protocol-error
          "not conform to the RESPONSE role within the INTROSPECTION protocol"
          "Malformed participant scope"
          "No participant-id scope component in scope")))

      (send-introspection-event "this will not work" :suffix-scope "foo")
      (check-conditions
       '((introspection-protocol-error
          "not conform to the RESPONSE role within the INTROSPECTION protocol"
          "Malformed participant scope"
          "Could not parse \"foo\" as UUID")))

      ;; Invalid payloads
      (send-introspection-event
       "this will not work" :suffix-scope (princ-to-string (uuid:make-null-uuid)))
      (check-conditions
       '((introspection-protocol-error
          "not conform to the RESPONSE role within the INTROSPECTION protocol"
          "Payload is" "\"this will not work\""
          "(not RSB.PROTOCOL.INTROSPECTION:HELLO or RSB.PROTOCOL.INTROSPECTION:BYE or \"pong\")")))

      (send-introspection-event
       1 :suffix-scope (princ-to-string (uuid:make-null-uuid)))
      (check-conditions
       '((introspection-protocol-error
          "not conform to the RESPONSE role within the INTROSPECTION protocol"
          "Payload is" "1"
          "(not RSB.PROTOCOL.INTROSPECTION:HELLO or RSB.PROTOCOL.INTROSPECTION:BYE or \"pong\")"))))))

(addtest (remote-introspection-root
          :documentation
          "Test change-hook of the `remote-introspection' class.")
  change-hook

  (let+ ((calls '())
         ((&flet record (&rest event)
            (appendf calls (list event))))
         ((&flet on-host-change (object subject event)
            (record object subject event)
            (flet ((hook () (database-change-hook subject)))
              (case event
                (:process-added   (hooks:add-to-hook (hook) (curry #'record subject)))
                (:process-removed (hooks:clear-hook (hook)))))))
         ((&flet on-database-change (subject event)
            (record :database subject event)
            (flet ((hook () (database-change-hook subject)))
              (case event
                (:host-added   (hooks:add-to-hook (hook) (curry #'on-host-change subject)))
                (:host-removed (hooks:clear-hook (hook)))))))
         ((&flet check-calls (expected)
            (let+ (((&flet+ check-call
                        ((call-object     call-subject     call-event)
                         (expected-object expected-subject expected-event))
                      (ensure (typep call-object  expected-object)
                              :report    "~@<Object ~A is not of type ~S~@:>"
                              :arguments (call-object expected-object))
                      (ensure (typep call-subject expected-subject)
                              :report    "~@<Subject ~A is not of type ~S~@:>"
                              :arguments (call-subject expected-subject))
                      (ensure (typep call-event   expected-event)
                              :report    "~@<Event ~A is not of type ~S~@:>"
                              :arguments (call-event expected-event))))
                   (num-calls    (length calls))
                   (num-expected (length expected)))
              (ensure-same num-calls num-expected
                           :report    "~@<~D call~:P but expected ~D~:@>"
                           :arguments (num-calls num-expected))
              (mapc #'check-call calls expected))
            (setf calls '()))))

    (with-participant (introspection (make-participant
                                      :remote-introspection +introspection-scope+
                                      :change-handler   #'on-database-change
                                      :update-interval  nil
                                      :response-timeout .01))
      ;; Create and destroy an `informer' participant and check the
      ;; generated events.
      (with-informer (informer "/rsbtest/remote-introspection/change-hook/participant" t)
        (declare (ignore informer)))
      (check-calls
       '(((eql :database) host-entry        (eql :host-added))
         (host-entry      process-entry     (eql :process-added))
         (process-entry   participant-entry (eql :participant-added))
         (process-entry   participant-entry (eql :participant-removed))
         (host-entry      process-entry     (eql :process-removed))
         (host-entry      (eql :unknown)    (eql :state-changed))))

      ;; Test clock-offset and latency events.
      (with-informer (informer "/rsbtest/remote-introspection/change-hook/timing" t)
        (declare (ignore informer))
        (setf calls '())
        (dotimes (i 4) (rsb.ep:handle introspection :update))
        (check-calls
         '((process-entry real (eql :clock-offset-changed))
           (process-entry real (eql :latency-changed))
           (host-entry    real (eql :clock-offset-changed))
           (host-entry    real (eql :latency-changed))))))))

(addtest (remote-introspection-root
          :documentation
          "Stress test which sends introspection events to a
           `remote-introspection' instance from multiple threads in
           parallel.")
  stress

  (with-participant (introspection
                     (make-participant
                      :remote-introspection +introspection-scope+
                      :update-interval .01))
    (with-participant (introspection
                       (make-participant
                        :local-introspection +introspection-scope+))
      (let+ ((configuration *configuration*)
             ((&flet participant-noise ()
                (let ((*configuration* configuration)
                      (*local-database* introspection))
                  (iter (repeat 30)
                        (sleep (random .01))
                        (with-local-server (server (string (gensym "/")))
                          (with-methods (server)
                              (("echo" (x) x) ("echo2" (x) x) ("echo3" (x) x))))))))
             (threads (map-into (make-list 10) (curry #'bt:make-thread
                                                      #'participant-noise))))
        (mapc #'bt:join-thread threads)))))