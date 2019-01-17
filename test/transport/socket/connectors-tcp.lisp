;;;; connectors-tcp.lisp --- Unit tests for TCP connector classes.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

;;; `connector' class

(def-suite* tcp-connector-root
  :in transport-socket-root
  :description
  "Unit tests for the `tcp-connector' class.")

(test tcp-connector/construction/smoke
  "Test constructing instances of the `tcp-connector' class."

  (flet ((do-it (&rest args)
           (apply #'make-instance 'tcp-connector
                  :schema    :tcp-socket
                  :host      "localhost"
                  :port      1
                  :converter nil
                  args)))
    (signals incompatible-initargs (do-it :server? nil :portfile "-"))
    (signals incompatible-initargs (do-it :server? :auto :portfile "-"))
    (signals error                 (do-it :if-leftover-connections :foo))
    (signals error                 (do-it :if-leftover-connections "foo"))

    (let ((instance (make-instance 'tcp-connector
                                   :schema    :tcp-socket
                                   :host      "foo"
                                   :port      1
                                   :converter nil)))
      (is (equal "foo" (connector-host instance)))
      (is (equal 1 (connector-port instance))))))

;;; Connector subclasses

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.socket "TCP-~A-CONNECTOR" direction))
             (suite-name (format-symbol *package* "TCP-~A-CONNECTOR-ROOT" direction)))
         `(progn
            (def-suite* ,suite-name
              :in transport-socket-root
              :description
              ,(format nil "Test suite for the `~(~A~)' class."
                       class-name))

            (define-basic-connector-test-cases ,class-name
              :name               :tcp-socket

              :initargs           (list :schema    :tcp-socket
                                        :host      "localhost"
                                        :port      *next-port*
                                        :converter :fundamental-null)

              :expected-schemas   '(:tcp-socket :socket)
              :expected-wire-type 'octet-vector
              :expected-remote?   t

              :expected-direction ,(make-keyword direction))

            (test ,(symbolicate class-name '#:/construct/invalid)
              ,(format nil "Test constructing `~(~A~)' instances."
                       class-name)

              ;; Missing :converter initarg.
              (signals missing-required-initarg
                (make-instance ',class-name)))))))

  (define-connector-suite :out)
  (define-connector-suite :in-pull)
  (define-connector-suite :in-push))
