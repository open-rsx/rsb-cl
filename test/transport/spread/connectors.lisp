;;;; connectors.lisp --- Unit tests for connector classes.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

;;; `in-connector' superclass

(def-suite* in-connector-root
  :in transport-spread-root
  :description
  "Tests for the `in-connector' class and associated methods.")

(test in-connector/notification->event
  "Test `notification->event' method."

  (mapc
   (lambda+ ((payload-data wire-schema connector-args expected))
     (let+ ((connector    (apply #'make-instance 'in-pull-connector
                                 :converter :fundamental-utf-8-string
                                 (append *common-args* connector-args)))
            (notification (rsb.transport.spread::make-incoming-notification
                           (a-notification 0 payload-data :wire-schema wire-schema)
                           payload-data))
            ((&flet do-it ()
               (rsb.ep:with-error-policy (connector)
                 (notification->event connector notification wire-schema)))))
       (case expected
         (decoding-error (signals decoding-error (do-it)))
         ((nil           (is (null (do-it)))))
         (t              (is (equal expected (event-data (do-it))))))))

   `(;; In these cases, protocol buffer unpacking fails.
     (,(octetify "foobarbaz") "foo"          (:error-policy nil)         decoding-error)
     (,(octetify "foobarbaz") "foo"          (:error-policy ,#'continue) nil)

     ;; Protocol buffer unpacking succeeds, but conversion to event
     ;; fails.
     (,(octetify "foobarbaz") "utf-8-string" ()                          "foobarbaz"))))

;;; Connector classes

(macrolet
    ((define-connector-suite (direction)
       (let+ ((class-name (format-symbol :rsb.transport.spread "~A-CONNECTOR" direction))
              (suite-name (format-symbol *package* "~A-CONNECTOR-ROOT" direction))
              ((&flet test-name (name)
                 (symbolicate class-name '#:/ name))))
         `(progn
            (def-suite* ,suite-name
              :in transport-spread-root
              :description
              ,(format nil "Test suite for the `~(~A~)' class."
                       class-name))

            (define-basic-connector-test-cases ,class-name
              :initargs           *common-args*

              :expected-schemas   '(:spread)
              :expected-wire-type 'octet-vector
              :expected-remote?   t

              :expected-direction ,(make-keyword direction))

            (test ,(test-name '#:construct/invalid)
              ,(format nil "Test constructing `~(~A~)' instances." class-name)

              ;; Missing :host, :port, :name or :bus initarg.
              (signals missing-required-initarg
                (make-instance ',class-name :schema    :spread
                                            :converter :fundamental-null
                                            :port      nil)))

            (test ,(test-name '#:connect/smoke)
              ,(format nil "Test connecting `~(~A~)' instances." class-name)

              (let ((connector (apply #'make-instance ',class-name *common-args*))
                    (scope     (make-scope "/foo")))
                (finishes (rsb.ep:notify connector scope :attached))
                (finishes (rsb.ep:notify connector scope :detached))))))))

  (define-connector-suite :out)
  (define-connector-suite :in-pull)
  (define-connector-suite :in-push))
