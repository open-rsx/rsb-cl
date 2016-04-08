;;;; server.lisp --- Unit tests for the method1 and server classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(def-suite method1-root
  :in patterns-request-reply-root
  :description
  "Test suite for `method1' class.")
(in-suite method1-root)

(test construction
  "Test constructing `method1' instances."

  (mapc
   (lambda+ ((initargs expected))
     (let+ (((&flet do-it ()
               (apply #'make-instance 'method1
                      :scope (make-scope "/") initargs))))
       (case expected
         (missing-required-initarg
          (signals missing-required-initarg (do-it)))
         (type-error
          (signals type-error (do-it)))
         (t
          (finishes (do-it))))))

   '( ;; Some invalid instantiations.
     (()                               missing-required-initarg) ; missing :name
     ((:name "foo")                    missing-required-initarg) ; missing :server
     ((:name "illegal/name" :server t) type-error)
     ((:name "illegal name" :server t) type-error)
     ((:name "i113g4l n4m3" :server t) type-error)

     ;; These are valid.
     ((:name "legal-name" :server t)   t))))

(def-suite server-root
  :in patterns-request-reply-root
  :description
  "Test suite for the `server' class.")
(in-suite server-root)

(test construction
  "Test constructing `server' instances."

  (mapc
   (lambda+ ((initargs &optional expected))
     (let+ (((&flet do-it ()
               (apply #'make-instance 'server initargs))))
       (case expected
         (missing-required-initarg
          (signals missing-required-initarg (do-it)))
         (type-error
          (signals type-error (do-it)))
         (t
          (finishes (do-it))))))

   `(;; Some invalid constructions.
     (()                            missing-required-initarg) ; scope
     ((:scope "/" :transform-option ,#'1+) type-error)
     ((:scope "/" :transform-option ,#'1+) type-error)

     ;; These are valid.
     ((:scope "/" :transform-option ((:return . ,#'1+))))
     ((:scope "/" :transform-option ((:argument . ,#'1+))))
     ((:scope "/" :transform-option ((:return . ,#'1+) (:argument . ,#'1+))))
     ((:scope "/" :transform-option ((:argument . ,#'1+) (:return . ,#'1+)))))))

(test set-method
  "Test adding methods to a `server' instance."

  (with-active-participants
      ((server (make-instance
                'server
                :scope "/rsbtest/patterns/request-reply/server-root/server"))
       (method (make-instance
                'method1
                :scope  (make-scope "/rsbtest/patterns/request-reply/server-root/server/foo")
                :name   "foo"
                :server server)))
    (mapc
     (lambda+ ((name method expected))
       (case expected
         (type-error
          (signals type-error (setf (server-method server name) method)))
         (t
          (let ((result-1 (setf (server-method server name) method))
                (result-2 (server-method server name :error? nil)))
            (is (eql expected result-1))
            (is (eql expected result-2))))))

     `(("foo"          ,method ,method)
       ("foo"          ,method ,method)
       (nil            ,method ,method)

       ("foo"          nil     nil)
       (nil            nil     nil)

       ;; invalid method name => type-error
       ("%invalidname" ,method type-error)))))
