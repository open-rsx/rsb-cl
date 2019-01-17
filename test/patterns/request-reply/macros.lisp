;;;; macros.lisp --- Unit tests for macros of the patterns module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(def-suite* macros-root
  :in patterns-request-reply-root
  :description
  "Root test suite for tests of macros in the patterns module.")

;;; Local server-related macros

(test with-methods/smoke
  "Smoke test for the `with-methods' macro."

  (with-participant (server :local-server "inprocess:/rsbtest/patterns/request-reply/macros-root/with-methods/smoke")
    (with-methods (server)
        ;; Normal method names
        (("mymethod"     (foo string) foo)
         (:myothermethod (bar integer)
          (declare (ignore bar)))
         ("noarg"        ())
         ("eventarg"     (baz :event)
           (declare (ignore baz)))
         ("notype"       (fez)
           (declare (ignore fez)))

         ;; Catch-all methods
         (nil (foo string) foo)
         (nil (bar integer)
            (declare (ignore bar)))
         (nil ())
         (nil (baz :event)
           (declare (ignore baz)))
         (nil (fez)
           (declare (ignore fez))))

      (is (not (null (server-method server "mymethod"))))
      (is (not (null (server-method server "MYOTHERMETHOD"))))
      (is (not (null (server-method server "noarg"))))
      (is (not (null (server-method server "eventarg"))))
      (is (not (null (server-method server "notype"))))

      (is (not (null (server-method server nil)))))
    (is (null (server-methods server)))))

(test with-methods/macroexpand
  "Test macroexpansion behavior of `with-methods' macro."

  (mapc (lambda+ ((method expected))
          (flet ((do-it () (macroexpand `(with-methods (server) (,method)))))
           (case expected
             (type-error (signals type-error (do-it)))
             (t          (finishes (do-it))))))

        '( ;; Some invalid constructions.
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
          (("noarg"        ()        :const) t)

          ;; Catch-all methods.
          ((nil            (foo string) foo) t)
          ((nil            (foo :event) foo) t)
          ((nil            ()        :const) t))))

(test with-participant/local-server/error-policy
  "Test handling of error-policy keyword parameter in
   `with-participant' macro when used with a :local-server
   participant."

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (local-server :local-server
                                            "inprocess:/rsbtest/patterns/request-reply/macros-root/with-local-server/error-policy"
                                            :transform `((:argument . ,#'mock-transform/error))
                                            ,@(when policy `(:error-policy ,policy)))
              (with-methods (local-server) (("echo" (arg) arg))
                (with-participant (remote-server :remote-server
                                                 "inprocess:/rsbtest/patterns/request-reply/macros-root/with-local-server/error-policy")
                  (call remote-server "echo" 1))))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (signals remote-call-error (test-case))

      ;; With `continue' error policy, calling the method should
      ;; proceed without the failing transformation.
        (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (is (= 1 result))
        (is (= 1 (length calls)))
        (is (typep (first calls) 'rsb.transform:transform-error))))))

;;; Remote server-related macros

(test with-participant/remote-server/error-policy
  "Test handling of error-policy keyword parameter in
   `with-participant' macro when used with a :remote-server
   participant."

  (let ((calls '()))
    (macrolet
        ((test-case (&optional policy)
           `(with-participant (remote-server :remote-server
                                             "inprocess:/rsbtest/patterns/request-reply/macros-root/with-remote-server/error-policy"
                                             :transform `((:return . ,#'mock-transform/error))
                                      ,@(when policy `(:error-policy ,policy)))
              (with-participant (local-server :local-server
                                              "inprocess:/rsbtest/patterns/request-reply/macros-root/with-remote-server/error-policy")
                (with-methods (local-server) (("echo" (arg) arg))
                  (call remote-server "echo" 1))))))

      ;; Without an error policy, the transform error should just be
      ;; signaled.
      (signals remote-call-error (test-case))

      ;; With `continue' error policy, calling the method should
      ;; proceed without the failing transformation.

      (let ((result (test-case (lambda (condition)
                                 (push condition calls)
                                 (continue condition)))))
        (is (= 1 result))
        (is (= 1 (length calls)))
        (is (typep (first calls) 'rsb.transform:transform-error))))))
