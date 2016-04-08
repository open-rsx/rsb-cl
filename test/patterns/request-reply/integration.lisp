;;;; integration.lisp --- Integration test for local and remote servers.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(def-suite integration-root
  :in patterns-request-reply-root
  :description
  "Integration test for `local-server' and `remote-server' classes.")
(in-suite integration-root)

(def-fixture with-url ()
  (let ((url "inprocess:/rsbtest/server/integration"))
    (&body)))

(test (smoke :fixture with-url)
  "Smoke test for communication between `local-server' and
   `remote-server' instances."

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("echo" (arg string)
           arg)
         ("nop" ()
           (values)))
      (with-participant (remote-server :remote-server url)
        (for-all ((s (gen-string)))

          ;; Invoke with payload result using `call' method
          (is (string= s (call remote-server "echo" s)))

          ;; Invoke with event result using `call' method
          (call remote-server "echo" s :return :event)

          ;; Invoke using `funcall'
          (is (string= s (funcall (server-method remote-server "echo") s)))

          ;; Invoke with event result using `funcall'
          (is (string= s (event-data
                          (funcall (server-method remote-server "echo")
                                   s :return :event)))))

        ;; Invoke without argument using `funcall'
        (funcall (server-method remote-server "nop"))))))

(test (smoke/event :fixture with-url)
  "Test communication in which the `local-method' and the
   `remote-method' operate on events."

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("echo" (arg :event) arg))
      (with-participant (remote-server :remote-server url)
        (let ((event (make-event
                      (merge-scopes
                       '("echo") (participant-scope remote-server))
                      5)))
          ;; Invoke with payload result using `call' method
          (is (= 5 (call remote-server "echo" event)))

          ;; Invoke with event result using `call' method
          (is (typep (call remote-server "echo" event :return :event) 'event))

          ;; Invoke using `funcall'
          (is (= (funcall (server-method remote-server "echo") event) 5))

          ;; Invoke with event result using `funcall'
          (is (typep (funcall (server-method remote-server "echo")
                              event :return :event)
                     'event)))))))

(test (local-catch-all-method :fixture with-url)
  "Test catch-all method on `local-server' side."

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        ((nil (arg integer) (1+ arg)))
      (with-participant (remote-server :remote-server url)
        (is (eql 2 (call remote-server "foo" 1)))
        (is (eql 4 (call remote-server "bar" 3)))))))

(test (remote-catch-all-method :fixture with-url)
  "Test catch-all method on `remote-server' side."

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("foo" (arg integer) (1+ arg)))
      (with-participant (remote-server :remote-server url)
        (let ((scope (merge-scopes '("foo") (uri->scope-and-options
                                             (puri:uri url)))))
          (is (eql 2 (call remote-server nil (make-event scope 1)))))))))

(test (error :fixture with-url)
  "Test calling a remote method which signals an error during
   execution."

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("error" (arg string)
           (declare (ignore arg))
           (error "intentional error")))
      (with-participant (remote-server :remote-server url)

        ;; Invoke using `call' method
        (signals remote-method-execution-error
          (call remote-server "error" "foo"))

        ;; Invoke asynchronously using `call' method
        (is (equal (list nil :failed)
                   (multiple-value-list
                    (future-result
                     (call remote-server "error" "foo" :block? nil)
                     :error? nil))))

        ;; Invoke using `funcall'
        (signals remote-method-execution-error
          (funcall (server-method remote-server "error") "foo"))

        ;; Invoke asynchronously using `funcall'
        (is (equal (list nil :failed)
                   (multiple-value-list
                    (future-result
                     (funcall (server-method remote-server "error")
                              "foo" :block? nil)
                     :error? nil))))))))

(test (timeout :fixture with-url)
  "Test timeout behavior when calling non-existent methods."

  (with-participant (remote-server :remote-server url)
    ;; Invoke using `call' method
    (signals bt:timeout
      (call remote-server "nosuchmethod" "does-not-matter"
            :timeout .1))
    (signals bt:timeout
      (future-result (call remote-server "nosuchmethod" "does-not-matter"
                           :block? nil)
                     :timeout .1))

    ;; Invoke using `funcall'
    (signals bt:timeout
      (funcall (server-method remote-server "nosuchmethod") "does-not-matter"
               :timeout .1))
    (signals bt:timeout
      (future-result (funcall (server-method remote-server "nosuchmethod")
                              "does-not-matter"
                              :block? nil)
                     :timeout .1))))

(test (transform :fixture with-url)
  "Test transforming events between `local-server' and `remote-server'
   instances."

  (let+ (((&flet event-1+ (event)
            (incf (event-data event))
            event))
         ((&flet event-2* (event)
            (setf (event-data event) (* 2 (event-data event)))
            event)))
    (with-participant (local-server :local-server url
                                    :transform `((:argument . ,#'event-1+)
                                                 (:return   . ,#'event-2*)))
      (with-methods (local-server)
          (("addfive" (arg integer)
             (+ arg 5)))
        (with-participant (remote-server :remote-server url
                                         :transform `((:argument . ,#'event-2*)
                                                      (:return   . ,#'event-1+)))
          (for-all ((i (gen-integer)))
            ;; Invoke with payload result using `call' method
            (is (= (1+ (* 2 (+ 5 (1+ (* 2 i)))))
                   (call remote-server "addfive" i)))))))))
