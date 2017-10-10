;;;; socket-tcp.lisp --- TCP socket Provider of the socket service.
;;;;
;;;; Copyright (C) 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket)

(defclass tcp-passive-stream-socket (usocket:stream-server-usocket)
  ((options :initarg  :options
            :reader   socket-%options
            :initform '())))

(defmethod usocket:socket-accept ((socket tcp-passive-stream-socket)
                                  &key element-type)
  (declare (ignore element-type))
  (when-let ((client-socket (call-next-method)))
    (let+ (((&plist-r/o (nodelay? :nodelay?)) (socket-%options socket)))
      (log:debug "~@<~A is enabling ~S socket option on accepted ~
                  socket ~A~@:>"
                 socket :tcp-nodelay client-socket)
      ;; Set requested TCPNODELAY behavior.
      (setf (usocket:socket-option client-socket :tcp-nodelay) nodelay?))
    client-socket))

(defun make-tcp-passive-socket
    (&key
    (host           (missing-required-argument :host))
    (port           (missing-required-argument :port))
    (backlog        5)
    (reuse-address? nil)
    (nodelay?       nil))
  (let ((socket (usocket:socket-listen host port
                                       :element-type  '(unsigned-byte 8)
                                       :backlog       backlog
                                       :reuse-address reuse-address?)))
    (if nodelay?
        (change-class socket 'tcp-passive-stream-socket
                      :options '(:nodelay? t))
        socket)))

(service-provider:register-provider/function
 'socket '(:tcp :passive) :function 'make-tcp-passive-socket)

(defun make-tcp-active-socket
    (&key
     (host     (missing-required-argument :host))
     (port     (missing-required-argument :port))
     (nodelay? nil))
  (usocket:socket-connect host port
                          :element-type '(unsigned-byte 8)
                          :nodelay      nodelay?))

(service-provider:register-provider/function
 'socket '(:tcp :active) :function 'make-tcp-active-socket)
