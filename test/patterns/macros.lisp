;;;; macros.lisp --- Unit tests for macros of the patterns module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.test)

(deftestsuite macros-root (patterns-root)
  ()
  (:documentation
   "Root test suite for tests of macros in the patterns module."))

(deftestsuite with-methods-root (macros-root)
  ()
  (:documentation
   "Test suite for the `with-methods' macro."))

(addtest (with-methods-root
          :documentation
          "Smoke test for the `with-methods' macro.")
  smoke

  (with-local-server (server "inprocess:")
    (with-methods (server)
        (("mymethod"     (foo string) foo)
         (:myothermethod (bar integer)
          (declare (ignore bar)))
         ("noarg"        ()))
      (ensure (server-method server "mymethod"))
      (ensure (server-method server "MYOTHERMETHOD"))
      (ensure (server-method server "noarg")))
    (ensure-null (server-methods server))))

(addtest (with-methods-root
          :documentation
          "Test macroexpansion behavior of `with-methods' macro.")
  macroexpand

  (ensure-cases (method expected)
      '(;; Some invalid constructions.
        (("invalid name" (foo string) foo) type-error)
        (("%invalidname" (foo string) foo) type-error)
        (("invalidtype"  (foo 5)      foo) type-error)

        ;; These are valid.
        (("validname"    (foo string) foo) t)
        ((:validname     (foo string) foo) t)
        ((validname      (foo string) foo) t)
        (("valid-name"   (foo string) foo) t)
        (("v41id_n4m3"   (foo string) foo) t)
        (("eventarg"     (foo :event) foo) t)
        (("noarg"        ()        :const) t))

    (case expected
      (type-error (ensure-condition 'type-error
                    (macroexpand `(with-methods (server) (,method)))))
      (t          (macroexpand `(with-methods (server) (,method)))))))
