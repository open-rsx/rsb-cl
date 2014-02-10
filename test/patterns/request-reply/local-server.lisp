;;;; local-server.lisp --- Unit tests for the local-server class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

(deftestsuite local-method-root (patterns-request-reply-root)
  ()
  (:documentation
   "Test suite for `local-method' class."))

(addtest (local-method-root
          :documentation
          "Test constructing `local-method' instances.")
  construction

  ;; Missing :callback initarg
  (ensure-condition 'missing-required-initarg
    (make-instance 'local-method :scope (make-scope "/foo") :name "foo")))

(deftestsuite local-server-root (patterns-root)
  ((simple-server (make-instance 'local-server
                                 :scope             "/localserver"
                                 :transport-options '((:inprocess)))))
  (:documentation
   "Test suite for the `local-server' class."))

(addtest (local-server-root
          :documentation
          "Test adding methods to a `local-server' instance.")
  set-method

  (ensure-cases (name method args expected)
      `(("foo"          ,(lambda ()) ()                   t)
        ("foo"          ,(lambda ()) ()                   t)
        ("foo"          nil            ()                   nil)

        ("bar"          ,(lambda ()) (:argument :event)   t)
        ("bar"          ,(lambda ()) (:argument :payload) t)

        ;; invalid method name => error
        ("%invalidname" ,(lambda ()) ()                   :error)
        ;; invalid argument style => error
        ("bar"          ,(lambda ()) (:argument :foo)     :error))

    (if (eq expected :error)
        (ensure-condition 'type-error
          (setf (apply #'server-method simple-server name args) method))
        (let ((result-1 (setf (apply #'server-method simple-server name args) method))
              (result-2 (server-method simple-server name
                                       :error? nil)))
          (if (eq expected t)
              (progn
                (ensure result-1)
                (ensure result-2))
              (progn
                (ensure-same result-1 expected)
                (ensure-same result-2 expected)))))))

(addtest (local-server-root
          :documentation
          "Test methods on `call' for the `local-server' class.")
  call

  (let ((argument))
    (setf (server-method simple-server "echopayload")
          (lambda (x) (setf argument x))
          (server-method simple-server "echoevent"
                         :argument :event)
          (lambda (x) (setf argument x) (event-data x))
          (server-method simple-server "error")
          (lambda (x) (error "intentional error")))

    (ensure-cases (method arg expected-argument expected-result)
        '(("echopayload" "foo" "foo"  "foo")
          ("echoevent"   "foo" event  "foo")
          ("error"       "foo" :none  string))

      (setf argument :none)
      (let* ((result      (call simple-server
                                (server-method simple-server method)
                                (make-event "/localserver" arg)))
             (result-data (event-data result)))
        (if (typep expected-argument '(and symbol (not keyword)))
            (ensure (typep argument expected-argument))
            (ensure-same argument expected-argument
                         :test #'equal))

        (ensure (typep result 'event))
        (if (symbolp expected-result)
            (ensure (typep result-data expected-result))
            (ensure-same result-data expected-result
                         :test #'equal))))))
