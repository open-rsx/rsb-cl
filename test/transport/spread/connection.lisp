;;;; connection.lisp --- Unit tests for the connection class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

(deftestsuite spread-connection-root (transport-spread-root)
  ()
  (:documentation
   "Unit tests for the `connection' class."))

(addtest (spread-connection-root
          :documentation
          "Test construction of `connection' instances.")
  construct

  (let+ ((name (format nil "~D" spread-port))
         ((&flet connect ()
            (network.spread:connect name))))
    (ensure-cases (initargs expected)
        `(;; Some invalid cases.
          (()                            missing-required-initarg) ; Neither :connection nor :name
          ((:name       "3333@localhost"
            :connection ,(connect))      incompatible-initargs)    ; Both :connection and :name

          ;; These are OK.
          ((:connection ,(connect))      t)
          ((:name ,name)                 t))
      (let+ (((&flet+ do-it ()
                (apply #'make-instance 'connection initargs))))
       (case expected
         (missing-required-initarg
          (ensure-condition 'missing-required-initarg (do-it)))
         (incompatible-initargs
          (ensure-condition 'incompatible-initargs (do-it)))
         (t
          (detach (do-it))))))))

(addtest (spread-connection-root
          :documentation
          "Test printing `connection' instances.")
  print

  (with-connection (connection :port spread-port)
    (ensure (not (emptyp (princ-to-string connection))))))
