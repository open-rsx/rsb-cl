;;;; conditions.lisp --- Conditions used in the socket transport.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(define-condition socket-bus-auto-connection-error (rsb-error
                                                    simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to connect to socket-based bus as ~
                     automatically determined client or server~@:>")
     (maybe-print-explanation stream condition)))
  (:documentation
   "This error is signaled when a an attempt to obtain a bus
provider in automatic client vs. server selection mode."))

(define-condition connection-shutdown-requested (condition)
  ((connection :initarg  :connection
               :type     t
               :reader   connection-shutdown-requested-connection
               :documentation
               "Stores the connection the shutdown of which has been
requested."))
  (:default-initargs
   :connection (missing-required-initarg
                'connection-shutdown-requested :connection))
  (:documentation
   "This condition is signaled when the remote peer requests
termination of the connection."))

(defmethod shutdown-handshake-for ((condition connection-shutdown-requested))
  :receive)
