;;;; protocol.lisp --- Unit tests for the protocol provided by the transform module.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform.test)

(def-suite* rsb.transform.transform!-root
  :in rsb.transform-root
  :description
  "Unit tests for the `transform!' generic function.")

(test default-behavior
  "Test default behavior of the `transform!' generic function."

  (mapc
   (lambda+ ((transform object expected))
     (let+ (((&flet do-it ()
               (transform! transform object))))
       (case expected
         (transform-error (signals transform-error (do-it)))
         (t               (is (eql expected (do-it)))))))

   `(;; Some invalid transforms.
     (:no-such-transform     :does-not-matter transform-error)
     ((:no-such-transform)   :does-not-matter transform-error)
     (,#'1+                  :wrong-type      transform-error)

     ;; These are valid.
     (,#'1+                  1                2)
     ((,#'1+ ,(curry #'* 2)) 1                3)
     ((,(curry #'* 2) ,#'1+) 1                4))))

(test restarts
  "Test restarts established by default methods on the `transform!'
   generic function."

  (mapc
   (lambda+ ((restart expected))
     (handler-bind
         ((error (lambda (condition)
                   (declare (ignore condition))
                   (let+ (((name &rest args) (ensure-list restart))
                          (restart (find-restart name)))
                     ;; Ensure it is there.
                     (is-true restart)
                     ;; Ensure it prints.
                     (is (typep (princ-to-string restart) 'string))
                     ;; Ensure it works.
                     (apply #'invoke-restart restart args)))))
       (is (eql expected (transform! (curry #'error "~@<I hate ~A~@:>") 1)))))

   '((continue      1)
     ((use-value 2) 2))))

(def-suite* rsb.transform.make-transform-root
  :in rsb.transform-root
  :description
  "Unit tests for the `make-transform' generic function.")

(test smoke
  "Smoke test for the `make-transform' generic function."

  (mapc (lambda+ ((spec expected))
          (let+ (((&flet do-it ()
                    (apply #'make-transform (ensure-list spec)))))
            (case expected
              (transform-creation-error
               (signals transform-creation-error (do-it)))
              (t
               (do-it)))))

        '((:no-such-transform transform-creation-error))))
