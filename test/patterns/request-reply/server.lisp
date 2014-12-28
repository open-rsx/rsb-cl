;;;; server.lisp --- Unit tests for the method1 and server classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(deftestsuite method1-root (patterns-request-reply-root)
  ()
  (:documentation
   "Test suite for `method1' class."))

(addtest (method1-root
          :documentation
          "Test constructing `method1' instances.")
  construction

  (ensure-cases (initargs expected)
      '(;; Some invalid instantiations.
        (()                               missing-required-initarg) ; missing :name
        ((:name "foo")                    missing-required-initarg) ; missing :server
        ((:name "illegal/name" :server t) type-error)
        ((:name "illegal name" :server t) type-error)
        ((:name "i113g4l n4m3" :server t) type-error)

        ;; These are valid.
        ((:name "legal-name" :server t)   t))

    (let+ (((&flet do-it ()
              (apply #'make-instance 'method1
                     :scope (make-scope "/") initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition 'missing-required-initarg (do-it)))
        (type-error
         (ensure-condition 'type-error (do-it)))
        (t
         (do-it))))))

(deftestsuite server-root (patterns-request-reply-root)
  ()
  (:documentation
   "Test suite for the `server' class."))

(addtest (server-root
          :documentation
          "Test constructing `server' instances.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid constructions.
        (()                            missing-required-initarg) ; scope
        ((:scope "/" :transform-option ,#'1+) type-error)
        ((:scope "/" :transform-option ,#'1+) type-error)

        ;; These are valid.
        ((:scope "/" :transform-option ((:return . ,#'1+))))
        ((:scope "/" :transform-option ((:argument . ,#'1+))))
        ((:scope "/" :transform-option ((:return . ,#'1+) (:argument . ,#'1+))))
        ((:scope "/" :transform-option ((:argument . ,#'1+) (:return . ,#'1+)))))

    (let+ (((&flet do-it ()
              (apply #'make-instance 'server initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition 'missing-required-initarg (do-it)))
        (type-error
         (ensure-condition 'type-error (do-it)))
        (t
         (do-it))))))

(addtest (server-root
          :documentation
          "Test adding methods to a `server' instance.")
  set-method

  (with-active-participants
      ((server (make-instance
                'server
                :scope "/rsbtest/patterns/request-reply/server-root/server"))
       (method (make-instance
                'method1
                :scope  (make-scope "/rsbtest/patterns/request-reply/server-root/server/foo")
                :name   "foo"
                :server server)))
    (ensure-cases (name method expected)
        `(("foo"          ,method ,method)
          ("foo"          ,method ,method)
          (nil            ,method ,method)

          ("foo"          nil     nil)
          (nil            nil     nil)

          ;; invalid method name => type-error
          ("%invalidname" ,method type-error))

      (case expected
        (type-error
         (ensure-condition 'type-error
           (setf (server-method server name) method)))

        (t
         (let ((result-1 (setf (server-method server name) method))
               (result-2 (server-method server name :error? nil)))
           (ensure-same result-1 expected)
           (ensure-same result-2 expected)))))))
