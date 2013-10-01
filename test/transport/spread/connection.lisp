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

  (let ((name (format nil "~D" spread-port)))
    (make-instance 'connection
                   :connection (network.spread:connect name))
    (make-instance 'connection
                   :name name)

    ;; Neither :connection nor :name => missing required initarg
    (ensure-condition 'missing-required-initarg
      (make-instance 'connection))
    ;; Both :connection and :name => mutually exclusive initargs
    (ensure-condition 'error
      (make-instance 'connection
                     :name       "3333@localhost"
                     :connection (network.spread:connect name)))))
