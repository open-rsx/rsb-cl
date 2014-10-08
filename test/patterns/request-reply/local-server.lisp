;;;; local-server.lisp --- Unit tests for the local-server class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

;;; `local-method' tests

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

;;; `local-server' tests

(deftestsuite local-server-root (patterns-request-reply-root
                                 participant-suite)
  ((simple-server (make-instance 'local-server
                                 :scope             "/localserver"
                                 :transport-options '((:inprocess :enabled t)))))
  (:documentation
   "Test suite for the `local-server' class."))

(define-basic-participant-test-cases (:local-server
                                      :check-transport-urls? nil)
  '("/localserver/construction"
    () () ()
    "/localserver/construction")

  '("inprocess://localhost/localserver/construction"
    () () ()
    "/localserver/construction")

  '("/localserver/construction?foo=bar"
    () () ()
    "/localserver/construction")

  ;; No transports => error
  '("/" () () (:transports ((t :enabled nil))) error))

(addtest (local-server-root
          :documentation
          "Test adding methods to a `local-server' instance.")
  set-method

  (ensure-cases (name method args expected)
      `(("foo"          ,(lambda ()) ()                   t)
        ("foo"          ,(lambda ()) ()                   t)
        ("foo"          nil          ()                   nil)

        (nil            ,(lambda ()) ()                   t)
        (nil            nil          ()                   nil)

        ("bar"          ,(lambda ()) (:argument :event)   t)
        ("bar"          ,(lambda ()) (:argument :payload) t)

        ;; invalid method name => type-error
        ("%invalidname" ,(lambda ()) ()                   type-error)
        ;; invalid argument style => type-error
        ("bar"          ,(lambda ()) (:argument :foo)     type-error))

    (let+ (((&flet do-it ()
              (values
               (setf (apply #'server-method simple-server name args) method)
               (server-method simple-server name :error? nil)))))
      (case expected
        (type-error (ensure-condition 'type-error (do-it)))
        ((t)        (ensure (notany #'null (multiple-value-list (do-it)))))
        (t          (ensure-same (do-it) (values expected expected)))))))

(addtest (local-server-root
          :documentation
          "Test methods on `call' for the `local-server' class.")
  call

  (let ((argument))
    (setf (server-method simple-server "echopayload")
          (lambda (x) (setf argument x))
          (server-method simple-server "echoevent" :argument :event)
          (lambda (x) (setf argument x) (event-data x))
          (server-method simple-server "error")
          (lambda (x) (error "intentional error")))

    (ensure-cases (method arg expected-argument expected-result)
        '(("echopayload" "foo" "foo"  "foo")
          ("echoevent"   "foo" event  "foo")
          ("error"       "foo" :none  string))

      (setf argument :none)
      (let* ((scope       (make-scope (list "localserver" method)))
             (result      (call simple-server
                                (server-method simple-server method)
                                (make-event scope arg)))
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
