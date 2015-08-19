;;;; protocol.lisp --- Tests for the protocol of the patterns module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.test)

(deftestsuite rsb-patterns-protocol-root (patterns-root)
  ()
  (:documentation
   "Unit tests for the protocol functions of the patterns module."))

(defclass mock-composite-participant/protocol (child-container-mixin) ())

(addtest (rsb-patterns-protocol-root
          :documentation
          "Smoke test for the `participant-child' generic function.")
  participant-child/smoke

  (let+ ((participant (make-instance 'mock-composite-participant/protocol))
         ((&flet do-it (&rest args)
            (apply #'participant-child participant :no-such :mock args))))
    ;; Try non-existing child, returning nil.
    (ensure-null (do-it))
    (ensure-null (do-it :if-exists nil)) ; ignored
    (ensure-null (do-it :detach? nil)) ; ignored
    (ensure-null (do-it :if-does-not-exist nil))

    ;; Try non-existing child, signaling an error.
    (ensure-condition 'no-such-child-error
      (do-it :if-does-not-exist 'error))
    (ensure-condition 'no-such-child-error
      (do-it :if-does-not-exist #'error))))

(addtest (rsb-patterns-protocol-root
          :documentation
          "Test `use-value' restart established by the
           `participant-child' generic function.")
  participant-child/restart

  (let ((participant (make-instance 'mock-composite-participant/protocol)))
    (handler-bind ((no-such-child-error
                    (lambda (condition)
                      (declare (ignore condition))
                      (let ((restart (find-restart 'use-value)))
                        (ensure (stringp (princ-to-string restart)))
                        (invoke-restart restart :replacement)))))
      (ensure-same (participant-child participant :no-such :mock
                                      :if-does-not-exist 'error)
                   :replacement))))

(addtest (rsb-patterns-protocol-root
          :documentation
          "Smoke test for the setf `participant-child' generic
           function.")
  setf-participant-child/smoke

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
    (ensure-same (put-it child1) child1 :test #'eq)
    (ensure-same (get-it)        child1 :test #'eq)

    (ensure-same (put-it child1) child1 :test #'eq)
    (ensure-same (get-it)        child1 :test #'eq)
    (ensure-same (mock-participant-state child1) :attached)

    (ensure-same (put-it child2) child2 :test #'eq)
    (ensure-same (get-it)        child2 :test #'eq)
    (ensure-same (mock-participant-state child1) :detached)

    ;; Replace child with :supersede.
    (ensure-same (put-it child1 :if-exists :supersede) child1 :test #'eq)
    (ensure-same (get-it)                              child1 :test #'eq)
    (ensure-same (mock-participant-state child2) :detached)

    ;; Remove child.
    (ensure-same (put-it nil) nil :test #'eq)
    (ensure-same (get-it)     nil :test #'eq)

    ;; Replace child without detaching it.
    (ensure-same (put-it child3 :if-exists :supersede)              child3
                 :test #'eq)
    (ensure-same (put-it child1 :if-exists :supersede :detach? nil) child1
                 :test #'eq)
    (ensure-same (mock-participant-state child3) :attached)

    ;; Signal an error for duplicate child.
    (ensure-same (put-it child3 :if-exists :supersede) child3 :test #'eq)
    (ensure-condition 'child-exists-error
      (put-it child2 :if-exists 'error))
    (ensure-same (get-it) child3 :test #'eq)
    (ensure-same (mock-participant-state child3) :attached)))

(addtest (rsb-patterns-protocol-root
          :documentation
          "Use `continue' restart established established by the setf
           `participant-child' generic function.")
  setf-participant-child/restart

  (let ((parent (make-instance 'mock-composite-participant/protocol))
        (child1 (make-participant :mock "/"))
        (child2 (make-participant :mock "/")))
    (handler-bind ((child-exists-error
                    (lambda (condition)
                      (declare (ignore condition))
                      (let ((restart (find-restart 'continue)))
                        (ensure (stringp (princ-to-string restart)))
                        (invoke-restart restart)))))
      (setf (participant-child parent :foo :mock)                   child1
            (participant-child parent :foo :mock :if-exists 'error) child2))
    (ensure-same (participant-child parent :foo :mock) child2 :test #'eq)))

(addtest (rsb-patterns-protocol-root
          :documentation
          "Smoke test for the `make-child-scope' generic function.")
  make-child-scope/smoke

  (ensure-cases (which expected)
      `((nil                               "/parent")
        (:foo                              "/parent/foo")
        ("foo"                             "/parent/foo")
        ("Foo"                             "/parent/Foo")
        ("FOO"                             "/parent/FOO")
        (,(make-scope "/foo")              "/parent/foo")
        (,(make-scope "/Foo")              "/parent/Foo")
        (,(make-scope "/FOO")              "/parent/FOO")
        (,(puri:uri "socket:/foo?baz=fez") ,(puri:uri "socket:/parent/foo/?baz=fez")))
    (let+ (((&flet do-it ()
              (let ((parent (make-participant :mock "/parent")))
                (make-child-scope parent which :listener)))))
      (ensure-same (do-it) expected
                   :test (lambda (left right)
                           (etypecase left
                             (scope    (scope= left right))
                             (puri:uri (puri:uri= left right))))))))

(addtest (rsb-patterns-protocol-root
          :documentation
          "Smoke test for the `make-child-participant' generic function.")
  make-child-participant/smoke

  (ensure-cases (initargs expected-kind expected-scope)
      '(;; Simplest case.
        (()                         :mock "/parent/foo")
        ;; Supply scope.
        ((:scope "/differentscope") :mock "/differentscope"))
    (let* ((parent (make-participant :mock "/parent"))
           (child  (apply #'make-child-participant parent "foo" :mock
                          initargs)))
      (ensure-same (participant-kind child) expected-kind)
      (ensure-same (participant-scope child) expected-scope :test #'scope=))))
