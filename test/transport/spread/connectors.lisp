;;;; connectors.lisp --- Unit tests for connector classes.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread.test)

;;; `in-connector' superclass

(deftestsuite in-connector-root (transport-spread-root)
  ((a-string           (let ((buffer (octetify "foobarbaz")))
                         (cons buffer (length buffer))))
   (empty-notification (let+ (((&values length buffer)
                               (pb:pack (make-instance
                                         'rsb.protocol:notification))))
                         (cons buffer length))))
  (:documentation
   "Tests for the `in-connector' class and associated methods."))

(addtest (in-connector-root
          :documentation
          "Test `notification->event' method.")
  notification->event

  (ensure-cases (notification wire-schema connector-args expected)
      `(;; In these cases, protocol buffer unpacking fails.
        (,a-string           :foo (:error-policy nil)          decoding-error)
        (,a-string           :foo (:error-policy ,#'continue)  nil)

        ;; Protocol buffer unpacking succeeds, but conversion to event
        ;; fails.
        (,empty-notification :foo (:error-policy nil)          decoding-error)
        (,empty-notification :foo (:error-policy ,#'continue)  nil))

    (let+ ((connector (apply #'make-instance 'in-pull-connector ; TODO(jmoringe): class
                             (append common-args connector-args)))
           ((&flet do-it ()
              (rsb.ep:with-error-policy (connector)
                (notification->event connector notification wire-schema)))))
      (case expected
        (decoding-error (ensure-condition 'decoding-error (do-it)))
        ((nil           (ensure-null (do-it))))))))

;;; Connector classes

(macrolet
    ((define-connector-suite (direction)
       (let ((class-name (format-symbol :rsb.transport.spread "~A-CONNECTOR" direction))
             (suite-name (format-symbol *package* "~A-CONNECTOR-ROOT" direction)))
         `(progn
            (deftestsuite ,suite-name (transport-spread-root)
              ()
              (:documentation
               ,(format nil "Test suite for the `~(~A~)' class."
                        class-name)))

            (define-basic-connector-test-cases ,class-name
              :initargs           common-args

              :expected-schemas   '(:spread)
              :expected-wire-type 'octet-vector

              :expected-direction ,(make-keyword direction))

            (addtest (,suite-name
                      :documentation
                      ,(format nil "Test constructing `~(~A~)' instances."
                               class-name))
              construct/invalid

              ;; Missing :host, :port, :name or :connection initarg.
              (ensure-condition 'missing-required-initarg
                (make-instance ',class-name)))))))

  (define-connector-suite :out)
  (define-connector-suite :in-pull)
  (define-connector-suite :in-push))
