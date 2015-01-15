;;;; integration.lisp --- Integration test for local and remote servers.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(deftestsuite integration-root (patterns-request-reply-root)
  ((url "inprocess:/rsbtest/server/integration"))
  (:documentation
   "Integration test for `local-server' and `remote-server'
classes."))

(addtest (integration-root
          :documentation
          "Smoke test for communication between `local-server' and
`remote-server' instances.")
  smoke

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("echo" (arg string)
            arg))
      (with-participant (remote-server :remote-server url)
        (ensure-random-cases 100 ((s a-string))

          ;; Invoke with payload result using `call' method
          (ensure-same (call remote-server "echo" s) s
                       :test #'string=)

          ;; Invoke with event result using `call' method
          (call remote-server "echo" s :return :event)

          ;; Invoke using `funcall'
          (ensure-same (funcall (server-method remote-server "echo") s) s
                       :test #'string=)

          ;; Invoke with event result using `funcall'
          (funcall (server-method remote-server "echo") s
                   :return :event))))))

(addtest (integration-root
          :documentation
          "Test communication in which the `local-method' and the
`remote-method' operate on events.")


  smoke/event

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("echo" (arg :event) arg))
      (with-participant (remote-server :remote-server url)
        (let ((event (make-event
                      (merge-scopes
                       '("echo") (participant-scope remote-server))
                      5)))
          ;; Invoke with payload result using `call' method
          (ensure-same (call remote-server "echo" event) 5 :test #'=)

          ;; Invoke with event result using `call' method
          (ensure (typep (call remote-server "echo"
                               event :return :event)
                         'event))

          ;; Invoke using `funcall'
          (ensure-same (funcall (server-method remote-server "echo") event)
                       5 :test #'=)

          ;; Invoke with event result using `funcall'
          (ensure (typep (funcall (server-method remote-server "echo")
                                  event :return :event)
                         'event)))))))

(addtest (integration-root
          :documentation
          "Test catch-all method on `local-server' side.")
  local-catch-all-method

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        ((nil (arg integer) (1+ arg)))
      (with-participant (remote-server :remote-server url)
        (ensure-same (call remote-server "foo" 1) 2)
        (ensure-same (call remote-server "bar" 3) 4)))))

(addtest (integration-root
          :documentation
          "Test catch-all method on `remote-server' side.")
  remote-catch-all-method

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("foo" (arg integer) (1+ arg)))
      (with-participant (remote-server :remote-server url)
        (let ((scope (merge-scopes '("foo") (uri->scope-and-options
                                             (puri:uri url)))))
          (ensure-same (call remote-server nil (make-event scope 1)) 2))))))

(addtest (integration-root
          :documentation
          "Test calling a remote method which signals an error during
execution.")
  error

  (with-participant (local-server :local-server url)
    (with-methods (local-server)
        (("error" (arg string)
           (error "intentional error")))
      (with-participant (remote-server :remote-server url)

        ;; Invoke using `call' method
        (ensure-condition 'remote-method-execution-error
          (call remote-server "error" "foo"))

        ;; Invoke asynchronously using `call' method
        (ensure-same (future-result
                      (call remote-server "error" "foo"
                            :block? nil)
                      :error? nil)
                     (values nil :failed))

        ;; Invoke using `funcall'
        (ensure-condition 'remote-method-execution-error
          (funcall (server-method remote-server "error") "foo"))

        ;; Invoke asynchronously using `funcall'
        (ensure-same (future-result
                      (funcall (server-method remote-server "error")
                               "foo"
                               :block? nil)
                      :error? nil)
                     (values nil :failed))))))

(addtest (integration-root
          :documentation
          "Test timeout behavior when calling non-existent methods.")
  timeout

  (with-participant (remote-server :remote-server url)
    ;; Invoke using `call' method
    (ensure-condition 'bt:timeout
      (call remote-server "nosuchmethod" "does-not-matter"
            :timeout .1))
    (ensure-condition 'bt:timeout
      (future-result (call remote-server "nosuchmethod" "does-not-matter"
                           :block? nil)
                     :timeout .1))

    ;; Invoke using `funcall'
    (ensure-condition 'bt:timeout
      (funcall (server-method remote-server "nosuchmethod") "does-not-matter"
               :timeout .1))
    (ensure-condition 'bt:timeout
      (future-result (funcall (server-method remote-server "nosuchmethod")
                              "does-not-matter"
                              :block? nil)
                     :timeout .1))))

(addtest (integration-root
          :documentation
          "Test transforming events between `local-server' and
`remote-server' instances.")
  transform

  (let+ (((&flet event-1+ (event)
            (incf (event-data event))
            event))
         ((&flet event-2* (event)
            (setf (event-data event) (* 2 (event-data event)))
            event)))
    (with-participant (local-server :local-server url
                                    :transform `((:argument ,#'event-1+)
                                                 (:return   ,#'event-2*)))
      (with-methods (local-server)
          (("addfive" (arg integer)
             (+ arg 5)))
        (with-participant (remote-server :remote-server url
                                         :transform `((:argument ,#'event-2*)
                                                      (:return   ,#'event-1+)))
          (ensure-random-cases 100 ((i an-integer))

            ;; Invoke with payload result using `call' method
            (ensure-same (call remote-server "addfive" i)
                         (1+ (* 2 (+ 5 (1+ (* 2 i)))))
                         :test #'=)))))))
