;;;; connector.lisp --- Unit tests for the connector class.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

(deftestsuite connector-root (transport-root)
  ()
  (:documentation
   "Unit test for the `connector' class."))

(addtest (connector-root
          :documentation
          "Test `connector-relative-url'.")
  connector-relative-url

  (ensure-cases (initargs scope expected-url)
      '(((:schema :mock :host "azurit")              "/foo/bar/"
         "mock://azurit/foo/bar/")

        ((:schema :mock :port 5003)                  "/foo/bar/"
         "mock://localhost:5003/foo/bar/")

        ((:schema :mock :host "azurit" :port 5003)   "/foo/bar/"
         "mock://azurit:5003/foo/bar/")

        ((:schema :spread :host "azurit")            "/foo/bar/"
         "spread://azurit/foo/bar/")

        ((:schema :spread :port 5003)                "/foo/bar/"
         "spread://localhost:5003/foo/bar/")

        ((:schema :spread :host "azurit" :port 5003) "/foo/bar/"
         "spread://azurit:5003/foo/bar/"))

    (let+ ((connector (apply #'make-instance 'connector initargs))
           ((&flet do-it (relative-part)
              (connector-relative-url connector relative-part))))
      (ensure-same (do-it scope)              (puri:uri expected-url)
                   :test #'puri:uri=)
      (ensure-same (do-it (make-scope scope)) (puri:uri expected-url)
                   :test #'puri:uri=)
      (ensure-same (do-it (puri:uri scope))   (puri:uri expected-url)
                   :test #'puri:uri=))))
