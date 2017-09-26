;;;; connectors.lisp --- Unit tests for connector classes.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.socket.test)

;;; `connector' class

(deftestsuite connector-root (transport-socket-root)
  ()
  (:documentation
   "Unit tests for the `connector' class."))

(addtest (connector-root
          :documentation
          "Test constructing instances of the `connector' class.")
  construction/smoke

  (flet ((do-it (server?)
           (make-instance 'connector
                          :schema   :socket
                          :host      "localhost"
                          :port      1
                          :converter nil
                          :server?   server?
                          :portfile  "-")))
    (ensure-condition 'incompatible-initargs (do-it nil))
    (ensure-condition 'incompatible-initargs (do-it :auto))))

;;; Connector subclasses

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.socket "~A-CONNECTOR" direction))
             (suite-name (format-symbol *package* "~A-CONNECTOR-ROOT" direction)))
        `(progn
           (deftestsuite ,suite-name (transport-socket-root)
             ()
             (:documentation
              ,(format nil "Test suite for the `~(~A~)' class."
                       class-name)))

           (define-basic-connector-test-cases ,class-name
             :initargs           (list :schema    :socket
                                       :host      "localhost"
                                       :port      *next-port*
                                       :converter :fundamental-null)
             :expected-schemas   '(:socket)
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
