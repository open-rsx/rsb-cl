;;;; connectors-tcp.lisp --- Unit tests for TCP connector classes.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

;;; `connector' class

(deftestsuite tcp-connector-root (transport-socket-root)
  ()
  (:documentation
   "Unit tests for the `tcp-connector' class."))

(addtest (tcp-connector-root
          :documentation
          "Test constructing instances of the `tcp-connector' class.")
  construction/smoke

  (flet ((do-it (&rest args)
           (apply #'make-instance 'tcp-connector
                  :schema    :tcp-socket
                  :host      "localhost"
                  :port      1
                  :converter nil
                  args)))
    (ensure-condition 'incompatible-initargs
      (do-it :server? nil :portfile "-"))
    (ensure-condition 'incompatible-initargs
      (do-it :server? :auto :portfile "-"))
    (ensure-condition 'error
      (do-it :if-leftover-connections :foo))
    (ensure-condition 'error
      (do-it :if-leftover-connections "foo")))

  (let ((instance (make-instance 'tcp-connector
                                 :schema    :tcp-socket
                                 :host      "foo"
                                 :port      1
                                 :converter nil)))
    (ensure-same (connector-host instance) "foo" :test #'equal)
    (ensure-same (connector-port instance) 1     :test #'eql)))

;;; Connector subclasses

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.socket "TCP-~A-CONNECTOR" direction))
             (suite-name (format-symbol *package* "TCP-~A-CONNECTOR-ROOT" direction)))
         `(progn
            (deftestsuite ,suite-name (transport-socket-root)
              ()
              (:documentation
               ,(format nil "Test suite for the `~(~A~)' class."
                        class-name)))

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

            (addtest (,suite-name
                      :documentation
                      ,(format nil "Test constructing `~(~A~)' instances."
                               class-name))
              construct/invalid

              ;; Missing :converter initarg.
              (ensure-condition 'missing-required-initarg
                (make-instance ',class-name)))))))

  (define-connector-suite :out)
  (define-connector-suite :in-pull)
  (define-connector-suite :in-push))
