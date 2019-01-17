;;;; reloading.lisp --- Unit tests for introspection database reloading.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection.test)

(def-suite* reloading-root
  :in introspection-root
  :description
  "Unit test suite for the reloading the introspection database.")

(test (reloading/smoke
       :fixture (with-configuration +introspection-configuration+))
  "Smoke test for reinitializing the introspection database."

  ;; The listener forces a `local-introspection' database. Otherwise
  ;; `reinitialize-introspection' would be a noop.
  (with-participant (nil :listener "inprocess:/rsbtest/introspection/reloading")
    (let+ (((&flet introspection-data ()
              (rsb.introspection::with-local-database (database)
                (values (rsb.introspection::introspection-host database)
                        (rsb.introspection::introspection-process database)))))
           ((&values old-host old-process) (introspection-data))
           ((&values new-host new-process) (progn
                                             (reinitialize-introspection)
                                             (introspection-data))))
      (is (not (eq new-host    old-host)))
      (is (not (eq new-process old-process))))))
