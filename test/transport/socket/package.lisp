;;;; package.lisp --- Package definition for tests of the transport.socket module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.socket.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions
   #:lift

   #:nibbles

   #:rsb
   #:rsb.event-processing
   #:rsb.transport
   #:rsb.transport.socket

   #:rsb.test
   #:rsb.transport.test)

  (:import-from #:rsb.transport.socket
   #:tcp-connector
   #:tcp-in-push-connector
   #:tcp-in-pull-connector
   #:tcp-out-connector

   #:bus-connectors
   #:bus-connections

   #:transport-ensure-bus)

  (:shadowing-import-from #:rsb.transport.socket
   #:connector)

  (:documentation
   "This package contains unit tests for the transport.socket
module."))

(cl:in-package #:rsb.transport.socket.test)

(defparameter *next-port* 12346
  "Stores a port number to use in the next test requiring a server
socket. Should be incremented after each use.")

(defun port-usable? (port &optional (host "localhost"))
  "Return non-nil, if a listen socket can be bound to HOST and PORT."
  (ignore-some-conditions (usocket:address-in-use-error)
    (usocket:with-socket-listener (socket host port)
      t)))

(deftestsuite transport-socket-root (transport-root)
  ()
  (:function
   (next-port ()
     (let ((old *next-port*))
       (iter (until (port-usable? *next-port*))
             (incf *next-port*))
       (unless (= old *next-port*)
         (format *lift-debug-output* "~&;; Port in use; Incrementing ~D -> ~D~&"
               old *next-port*)))
     *next-port*))
  (:function
   (make-socket-url (schema server? options)
     (format nil "~(~A~):~{~@?~}/rsbtest/transport/socket~@[?~{~A=~A~^&~}~]"
             schema
             (ecase schema
               ((:socket :tcp-socket)
                (list "//localhost:~D" *next-port*))
               ((:unix :unix-socket)
                (list "")))
             (append (when server? (list "server" "1")) options))))
  ;; Prior to running each individual test case, clear bus clients and
  ;; servers and choose a new port.
  (:setup
   (let ((transport (service-provider:find-provider
                     'rsb.transport:transport :socket)))
     (clrhash (rsb.transport.socket::transport-clients transport))
     (clrhash (rsb.transport.socket::transport-servers transport)))
   (next-port))
  (:run-setup :once-per-test-case)
  (:documentation
   "Root unit test suite for the transport.socket module."))
