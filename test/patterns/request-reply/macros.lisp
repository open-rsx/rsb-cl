;;;; macros.lisp --- Unit tests for macros of the patterns module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(deftestsuite macros-root (patterns-request-reply-root)
  ()
  (:documentation
   "Root test suite for tests of macros in the patterns module."))

;;; Local server-related macros

(addtest (macros-root
          :documentation
          "Smoke test for the `with-methods' macro.")
  with-methods/smoke

  (with-local-server (server "inprocess:")
    (with-methods (server)
        (("mymethod"     (foo string) foo)
         (:myothermethod (bar integer)
          (declare (ignore bar)))
         ("noarg"        ())
         ("eventarg"     (baz :event)
           (declare (ignore baz)))
         ("notype"       (fez)
           (declare (ignore fez))))
      (ensure (server-method server "mymethod"))
      (ensure (server-method server "MYOTHERMETHOD"))
      (ensure (server-method server "noarg"))
      (ensure (server-method server "eventarg"))
      (ensure (server-method server "notype")))
    (ensure-null (server-methods server))))

(addtest (macros-root
          :documentation
          "Test macroexpansion behavior of `with-methods' macro.")
  with-methods/macroexpand

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

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-local-server' macro.")
  with-local-server/error-policy

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-local-server (local-server
                                "socket:"
                                :transform `((:argument . ,#'mock-transform/error))
                                ,@(when policy `(:error-policy ,policy)))
              (with-methods (local-server) (("echo" (arg) arg))
                (with-remote-server (remote-server "socket:")
                  (call remote-server "echo" 1))))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition remote-call-error
        (test-case))

      ;; With `continue' error policy, calling the method should
      ;; proceed without the failing transformation.
      #+TODO-enable-when-fixed
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure-same result 1)
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.event-processing:transform-error))))))

;;; Remote server-related macros

(addtest (macros-root
          :documentation
          "Test handling of error-policy keyword parameter in
           `with-remote-server' macro.")
  with-remote-server/error-policy

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-remote-server (remote-server
                                 "socket:"
                                 :transform `((:return . ,#'mock-transform/error))
                                 ,@(when policy `(:error-policy ,policy)))
              (with-local-server (local-server "socket:")
                (with-methods (local-server) (("echo" (arg) arg))
                  (call remote-server "echo" 1))))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (ensure-condition remote-call-error
        (test-case))

      ;; With `continue' error policy, calling the method should
      ;; proceed without the failing transformation.
      #+TODO-enable-when-fixed
      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (ensure-same result 1)
        (ensure-same (length calls) 1)
        (ensure (typep (first calls) 'rsb.event-processing:transform-error))))))
