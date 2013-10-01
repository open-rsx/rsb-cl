;;;; server.lisp --- Unit tests for the method1 and server classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.test)

(deftestsuite method1-root (patterns-root)
  ()
  (:documentation
   "Test suite for `method1' class."))

(addtest (method1-root
          :documentation
          "Test constructing `method1' instances.")
  construction

  (ensure-cases (initargs expected)
      '(;; Some invalid instantiations.
        (()                     missing-required-initarg) ; missing :name
        ((:name "illegal/name") type-error)
        ((:name "illegal name") type-error)
        ((:name "i113g4l n4m3") type-error)

        ;; These are valid.
        ((:name "legal-name")   t))

    (let+ (((&flet do-it () (apply #'make-instance 'method1 initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition 'missing-required-initarg (do-it)))
        (type-error
         (ensure-condition 'type-error (do-it)))
        (t
         (do-it))))))

(deftestsuite server-root (patterns-root)
  ((simple-server (make-instance 'server :scope "/server"))
   (simple-method (make-instance 'method1 :name "foo")))
  (:documentation
   "Test suite for the `server' class."))

(addtest (server-root
          :documentation
          "Test constructing `server' instances.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid constructions.
        (()                            missing-required-initarg) ; scope
        ((:scope "/" :transform ,#'1+) type-error)
        ((:scope "/" :transform ,#'1+) type-error)

        ;; These are valid.
        ((:scope "/" :transform ((:return ,#'1+))))
        ((:scope "/" :transform ((:argument ,#'1+))))
        ((:scope "/" :transform ((:return ,#'1+ :argument ,#'1+))))
        ((:scope "/" :transform ((:argument ,#'1+ :return ,#'1+)))))

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

  (ensure-cases (name method expected)
      `(("foo"          ,simple-method ,simple-method)
        ("foo"          ,simple-method ,simple-method)
        ("foo"          nil            nil)

        ;; invalid method name => error
        ("%invalidname" ,simple-method :error))

    (if (eq expected :error)
        (ensure-condition 'type-error
          (setf (server-method simple-server name) method))

        (let ((result-1 (setf (server-method simple-server name) method))
              (result-2 (server-method simple-server name
                                       :error? nil)))
          (ensure-same result-1 expected)
          (ensure-same result-2 expected)))))
