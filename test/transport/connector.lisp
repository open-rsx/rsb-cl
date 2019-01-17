;;;; connector.lisp --- Unit tests for the connector class.
;;;;
;;;; Copyright (C) 2011-2017, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

(def-suite* connector-root
  :in transport-root
  :description
  "Unit test for the `connector' class.")

(test connector-relative-url
  "Test `connector-relative-url'."

  (mapc
   (lambda+ ((initargs scope expected-url))
     (let+ ((connector (apply #'make-instance 'connector initargs))
            ((&flet do-it (relative-part)
               (connector-relative-url connector relative-part))))
       (is (puri:uri= (puri:uri expected-url) (do-it scope)))
       (is (puri:uri= (puri:uri expected-url) (do-it (make-scope scope))))
       (is (puri:uri= (puri:uri expected-url) (do-it (puri:uri scope))))))

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
      "spread://azurit:5003/foo/bar/"))))
