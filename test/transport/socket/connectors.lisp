;;;; connectors.lisp --- Unit tests for connector classes.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

;;; Connector classes

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.socket "~A-CONNECTOR" direction))
             (suite-name (format-symbol *package* "~A-CONNECTOR-ROOT" direction)))
        `(progn
           (deftestsuite ,suite-name (transport-socket-root
                                      connector-suite)
             ()
             (:documentation
              ,(format nil "Test suite for the `~(~A~)' class."
                       class-name)))

           (define-basic-connector-test-cases ,class-name
             :initargs           (list :host      "localhost"
                                       :port      *next-port*
                                       :converter :fundamental-null)
             :expected-direction ,(make-keyword direction)
             :expected-wire-type 'octet-vector
             :expected-schemas   '(:socket))

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
