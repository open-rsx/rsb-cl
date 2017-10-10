;;;; connectors.lisp --- Unit tests for UNIX connector classes.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

;;; `connector' class

(deftestsuite unix-connector-root (transport-socket-root)
  ()
  (:documentation
   "Unit tests for the `unix-connector' class."))

(addtest (unix-connector-root
          :documentation
          "Test constructing instances of the `unix-connector' class.")
  construction/smoke

  (flet ((do-it (server?)
           (make-instance 'unix-connector
                          :schema    :unix-socket
                          :name      "foo"
                          :converter nil
                          :server?   server?)))
    (do-it nil)
    (do-it :auto)))

;;; Connector subclasses

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.socket "UNIX-~A-CONNECTOR" direction))
             (suite-name (format-symbol *package* "UNIX-~A-CONNECTOR-ROOT" direction)))
         `(progn
            (deftestsuite ,suite-name (transport-socket-root)
              ()
              (:documentation
               ,(format nil "Test suite for the `~(~A~)' class."
                        class-name)))

            (define-basic-connector-test-cases ,class-name
              :name               :unix-socket

              :initargs           (list :schema    :unix
                                        :name      "foo"
                                        :converter :fundamental-null)

              :expected-schemas   '(:unix :unix-socket)
              :expected-wire-type 'octet-vector
              :expected-remote?   nil

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
