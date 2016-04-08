;;;; connectors.lisp --- Unit tests for UNIX connector classes.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

;;; `connector' class

(def-suite unix-connector-root
  :in transport-socket-root
  :describe
  "Unit tests for the `unix-connector' class.")
(in-suite unix-connector-root)

(test construction/smoke
  "Test constructing instances of the `unix-connector' class."

  (flet ((do-it (&rest args)
           (apply #'make-instance 'unix-connector
                  :schema    :unix-socket
                  :name      "foo"
                  :converter nil
                  args)))
    (ensure-condition 'error
      (do-it :if-leftover-connections :foo))
    (ensure-condition 'error
      (do-it :if-leftover-connections "foo")))

  (flet ((do-it (server?)
           (make-instance 'unix-connector
                          :schema    :unix-socket
                          :name      "foo"
                          :converter nil
                          :server?   server?)))
    (finishes (do-it nil))
    (finishes (do-it :auto))))

;;; Connector subclasses

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.socket "UNIX-~A-CONNECTOR" direction))
             (suite-name (format-symbol *package* "UNIX-~A-CONNECTOR-ROOT" direction)))
         `(progn
            (def-suite ,suite-name (transport-socket-root)
              ()
              (:documentation
               ,(format nil "Test suite for the `~(~A~)' class."
                        class-name)))
            (in-suite ,suite-name)

            (define-basic-connector-test-cases ,class-name
              :name               :unix-socket

              :initargs           (list :schema    :unix
                                        :name      "foo"
                                        :converter :fundamental-null)

              :expected-schemas   '(:unix :unix-socket)
              :expected-wire-type 'octet-vector
              :expected-remote?   nil

              :expected-direction ,(make-keyword direction))

            (test construct/invalid
              ,(format nil "Test constructing `~(~A~)' instances."
                           class-name)

              ;; Missing :converter initarg.
              (signals missing-required-initarg
                (make-instance ',class-name)))))))

  (define-connector-suite :out)
  (define-connector-suite :in-pull)
  (define-connector-suite :in-push))
