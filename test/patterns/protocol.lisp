;;;; protocol.lisp --- Tests for the protocol of the patterns module.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.test)

(def-suite* rsb-patterns-protocol-root
  :in patterns-root
  :description
  "Unit tests for the protocol functions of the patterns module.")

(defclass mock-composite-participant/protocol (child-container-mixin) ())

(test participant-child/smoke
  "Smoke test for the `participant-child' generic function."

  (let+ ((participant (make-instance 'mock-composite-participant/protocol))
         ((&flet do-it (&rest args)
            (apply #'participant-child participant :no-such :mock args))))
    ;; Try non-existing child, returning nil.
    (is (null (do-it)))
    (is (null (do-it :if-exists nil)))  ; ignored
    (is (null (do-it :detach? nil)))    ; ignored
    (is (null (do-it :if-does-not-exist nil)))

    ;; Try non-existing child, signaling an error.
    (signals no-such-child-error (do-it :if-does-not-exist 'error))
    (signals no-such-child-error (do-it :if-does-not-exist #'error))))

(test participant-child/restart
  "Test `use-value' restart established by the `participant-child'
   generic function."

  (let ((participant (make-instance 'mock-composite-participant/protocol)))
    (handler-bind ((no-such-child-error
                    (lambda (condition)
                      (declare (ignore condition))
                      (let ((restart (find-restart 'use-value)))
                        (is (stringp (princ-to-string restart)))
                        (invoke-restart restart :replacement)))))
      (is (eq :replacement (participant-child participant :no-such :mock
                                              :if-does-not-exist 'error))))))

(test setf-participant-child/smoke
  "Smoke test for the setf `participant-child' generic function."

  (let+ ((participant (make-instance 'mock-composite-participant/protocol))
         (child1      (make-participant :mock "/" :introspection? nil))
         (child2      (make-participant :mock "/" :introspection? nil))
         (child3      (make-participant :mock "/" :introspection? nil))
         ((&flet put-it (child &rest args)
            (apply '(setf participant-child) child participant :foo :mock
                   args)))
         ((&flet get-it (&rest args)
            (apply #'participant-child participant :foo :mock args))))
    ;; Install and replace child. Replacing a child with itself should
    ;; not detach.
    (is (eq child1    (put-it child1)))
    (is (eq child1    (get-it)))

    (is (eq child1    (put-it child1)))
    (is (eq child1    (get-it)))
    (is (eq :attached (mock-participant-state child1)))

    (is (eq child2    (put-it child2)))
    (is (eq child2    (get-it)))
    (is (eq :detached (mock-participant-state child1)))

    ;; Replace child with :supersede.
    (is (eq child1    (put-it child1 :if-exists :supersede)))
    (is (eq child1    (get-it)))
    (is (eq :detached (mock-participant-state child2)))

    ;; Remove child.
    (is (eq nil       (put-it nil)))
    (is (eq nil       (get-it)))

    ;; Replace child without detaching it.
    (is (eq child3    (put-it child3 :if-exists :supersede)))
    (is (eq child1    (put-it child1 :if-exists :supersede :detach? nil)))
    (is (eq :attached (mock-participant-state child3)))

    ;; Signal an error for duplicate child.
    (is (eq child3    (put-it child3 :if-exists :supersede)))
    (signals child-exists-error (put-it child2 :if-exists 'error))
    (is (eq child3    (get-it)))
    (is (eq :attached (mock-participant-state child3)))))

(test setf-participant-child/restart
  "Use `continue' restart established established by the setf
   `participant-child' generic function."

  (let ((parent (make-instance 'mock-composite-participant/protocol))
        (child1 (make-participant :mock "/"))
        (child2 (make-participant :mock "/")))
    (handler-bind ((child-exists-error
                    (lambda (condition)
                      (declare (ignore condition))
                      (let ((restart (find-restart 'continue)))
                        (is (stringp (princ-to-string restart)))
                        (invoke-restart restart)))))
      (setf (participant-child parent :foo :mock)                   child1
            (participant-child parent :foo :mock :if-exists 'error) child2))
    (is (eq child2 (participant-child parent :foo :mock)))))

(test make-child-scope/smoke
  "Smoke test for the `make-child-scope' generic function."

  (mapc
   (lambda+ ((which expected))
     (let+ (((&flet do-it ()
               (let ((parent (make-participant :mock "/parent")))
                 (make-child-scope parent which :listener)))))
       (is ((lambda (left right)
              (etypecase right
                (scope    (scope= left right))
                (puri:uri (puri:uri= left right))))
            expected (do-it)))))
   `((nil                               "/parent")
     (:foo                              "/parent/foo")
     ("foo"                             "/parent/foo")
     ("Foo"                             "/parent/Foo")
     ("FOO"                             "/parent/FOO")
     (,(make-scope "/foo")              "/parent/foo")
     (,(make-scope "/Foo")              "/parent/Foo")
     (,(make-scope "/FOO")              "/parent/FOO")
     (,(puri:uri "socket:/foo?baz=fez") ,(puri:uri "socket:/parent/foo/?baz=fez")))))

(test make-child-participant/smoke
  "Smoke test for the `make-child-participant' generic function."

  (mapc
   (lambda+ ((initargs expected-kind expected-scope))
     (let* ((parent (make-participant :mock "/parent"))
            (child  (apply #'make-child-participant parent "foo" :mock
                           initargs)))
       (is (eq     expected-kind  (participant-kind child)))
       (is (scope= expected-scope (participant-scope child)))))
   '(;; Simplest case.
     (()                         :mock "/parent/foo")
     ;; Supply scope.
     ((:scope "/differentscope") :mock "/differentscope"))))
