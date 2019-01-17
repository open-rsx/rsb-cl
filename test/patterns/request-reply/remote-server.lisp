;;;; remote-server.lisp --- Unit tests for the remote-server class.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

;;; `remote-method' tests

(def-suite* remote-method-root
  :in patterns-request-reply-root
  :description
  "Test suite for `remote-method' class.")

(test remote-method/construction
  "Test constructing `remote-method' instances."

  (make-instance 'remote-method
                 :scope  "/remoteserver/foo"
                 :name   "foo"
                 :server t))

;;; `remote-server' tests

(def-suite* remote-server-root
  :in patterns-request-reply-root
  :description
  "Unit tests for the `remote-server' class.")

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

(test remote-server/set-method
  "Test adding methods to a `remote-server' instance."

  (with-participant (server :remote-server "/rsbtest/remoteserver/set-method")
    (mapc (lambda+ ((name expected))
            (let+ (((&flet do-it ()
                      (server-method server name))))
              (case expected
                (type-error (signals type-error (do-it)))
                ((t)        (is (not (null (do-it))))))))

          `(("foo"          t)
            (nil            t)

            ;; invalid method name => error
            ("%invalidname" type-error)))))
