;;;; socket-unix.lisp --- UNIX domain socket provider of the socket service.
;;;;
;;;; Copyright (C) 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defun %make-local/abstract-socket (name)
  (values (make-instance 'sb-bsd-sockets:local-abstract-socket :type :stream)
          name))

(defun make-unix-passive-socket
    (&key
     (name    (missing-required-argument :name))
     (backlog 5))
  (let+ (((&values socket address) (%make-local/abstract-socket name)))
    ;; Bind to ADDRESS which is a name in the "abstract" namespace.
    (sb-bsd-sockets:socket-bind socket address)
    ;; Start listening.
    (sb-bsd-sockets:socket-listen socket backlog)
    ;; Finally construct and return an usocket wrapper.
    (usocket::make-stream-server-socket socket :element-type '(unsigned-byte 8))))

(service-provider:register-provider/function
 'socket '(:local/abstract :passive) :function 'make-unix-passive-socket)

(defun make-unix-active-socket
    (&key
     (name (missing-required-argument :name)))
  (let+ (((&values socket address) (%make-local/abstract-socket name))
         (stream (usocket:with-mapped-conditions (socket)
                   (sb-bsd-sockets:socket-connect socket address)
                   (sb-bsd-sockets:socket-make-stream
                    socket :input t :output t :buffering :full
                    :element-type '(unsigned-byte 8)
                    :serve-events nil))))
    ;; Finally construct and return an usocket wrapper.
    (usocket::make-stream-socket :socket socket :stream stream)))

(service-provider:register-provider/function
 'socket '(:local/abstract :active) :function 'make-unix-active-socket)
