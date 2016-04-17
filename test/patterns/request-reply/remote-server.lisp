;;;; remote-server.lisp --- Unit tests for the remote-server class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

;;; `remote-method' tests

(deftestsuite remote-method-root (patterns-request-reply-root)
  ()
  (:documentation
   "Test suite for `remote-method' class."))

(addtest (remote-method-root
          :documentation
          "Test constructing `remote-method' instances.")
  construction

  (make-instance 'remote-method
                 :scope  "/remoteserver/foo"
                 :name   "foo"
                 :server t))

;;; `remote-server' tests

(deftestsuite remote-server-root (patterns-request-reply-root
                                  participant-suite)
  ()
  (:documentation
   "Unit tests for the `remote-server' class."))

(define-basic-participant-test-cases (:remote-server
                                      :check-transport-urls? nil)
  '("/rsbtest/remoteserver/construction"
    ()
    "/rsbtest/remoteserver/construction")

  '("/rsbtest/remoteserver/construction"
    (:transports ((:inprocess &inherit)))
    "/rsbtest/remoteserver/construction")

  '("/rsbtest/remoteserver/construction"
    (:transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/remoteserver/construction")

  `("/rsbtest/remoteserver/construction"
    (:parent ,*simple-parent*)
    "/rsbtest/remoteserver/construction")

  '("/rsbtest/remoteserver/construction"
    (:introspection? nil)
    "/rsbtest/remoteserver/construction")

  '("/rsbtest/remoteserver/construction"
    (:introspection? t)
    "/rsbtest/remoteserver/construction")

  '("inprocess://localhost/rsbtest/remoteserver/construction"
    ()
    "/rsbtest/remoteserver/construction")

  '("/rsbtest/remoteserver/construction?foo=bar"
    ()
    "/rsbtest/remoteserver/construction")

  ;; No transports => error
  '("/" (:transports ((t :enabled nil))) error))

(addtest (remote-server-root
          :documentation
          "Test adding methods to a `remote-server' instance.")
  set-method

  (with-participant (server :remote-server "/rsbtest/remoteserver/set-method")
    (ensure-cases (name expected)
        `(("foo"          t)
          (nil            t)

          ;; invalid method name => error
          ("%invalidname" type-error))

      (let+ (((&flet do-it ()
                (server-method server name))))
        (case expected
          (type-error (ensure-condition 'type-error (do-it)))
          ((t)        (ensure (do-it))))))))
