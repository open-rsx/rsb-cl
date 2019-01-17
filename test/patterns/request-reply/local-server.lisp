;;;; local-server.lisp --- Unit tests for the local-server class.
;;;;
;;;; Copyright (C) 2011-2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply.test)

;;; `local-method' tests

(def-suite* local-method-root
  :in patterns-request-reply-root
  :description
  "Test suite for `local-method' class.")

(test local-method/construction
  "Test constructing `local-method' instances."

  ;; Missing :callback initarg
  (signals missing-required-initarg
    (make-instance 'local-method :scope (make-scope "/foo") :name "foo")))

;;; `local-server' tests

(def-suite* local-server-root
  :in patterns-request-reply-root
  :description
  "Test suite for the `local-server' class.")

(define-basic-participant-test-cases (:local-server
                                      :check-transport-urls? nil)
  '("/rsbtest/localserver/construction"
    ()
    "/rsbtest/localserver/construction")

  '("/rsbtest/localserver/construction"
    (:transports ((:inprocess &inherit)))
    "/rsbtest/localserver/construction")

  '("/rsbtest/localserver/construction"
    (:transports ((t &inherit) (:inprocess &inherit)))
    "/rsbtest/localserver/construction")

  `("/rsbtest/localserver/construction"
    (:parent ,*simple-parent*)
    "/rsbtest/localserver/construction")

  '("/rsbtest/localserver/construction"
    (:introspection? nil)
    "/rsbtest/localserver/construction")

  '("/rsbtest/localserver/construction"
    (:introspection? t)
    "/rsbtest/localserver/construction")

  '("inprocess://localhost/rsbtest/localserver/construction"
    ()
    "/rsbtest/localserver/construction")

  '("/rsbtest/localserver/construction?foo=bar"
    ()
    "/rsbtest/localserver/construction")

  ;; No transports => error
  '("/" (:transports ((t :enabled nil))) error))

(test (local-server/set-method :fixture with-configuration)
  "Test adding methods to a `local-server' instance."

  (with-participant (server :local-server "/rsbtest/localserver/set-method")
    (mapc
     (lambda+ ((name method args expected))
       (let+ (((&flet do-it ()
                 (values
                  (setf (apply #'server-method server name args) method)
                  (server-method server name :error? nil)))))
         (case expected
           (type-error (signals type-error (do-it)))
           ((t)        (is (notany #'null (multiple-value-list (do-it)))))
           (t          (is (equal (list expected expected)
                                  (multiple-value-list (do-it))))))))

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
       ("bar"          ,(lambda ()) (:argument :foo)     type-error)))))

(test (call :fixture with-configuration)
  "Test methods on `call' for the `local-server' class."

  (with-participant (server :local-server "/rsbtest/localserver/call")
    (let ((argument))
      (setf (server-method server "echopayload")
            (lambda (x) (setf argument x))
            (server-method server "echoevent" :argument :event)
            (lambda (x) (setf argument x) (event-data x))
            (server-method server "error")
            (lambda (x) (declare (ignore x)) (error "intentional error")))

      (mapc
       (lambda+ ((method arg expected-argument expected-result))
         (setf argument :none)
         (let* ((scope       (merge-scopes
                              (list method) "/rsbtest/localserver/call"))
                (event       (let ((event (make-event scope arg)))
                               (setf (event-sequence-number event)
                                     0
                                     (event-origin event)
                                     (uuid:make-v4-uuid))
                               event))
                (result      (call server (server-method server method) event))
                (result-data (event-data result)))
           (if (typep expected-argument '(and symbol (not keyword)))
               (is (typep argument expected-argument))
               (is (equal expected-argument argument)))

           (is (typep result 'event))
           (if (symbolp expected-result)
               (is (typep result-data expected-result))
               (is (equal expected-result result-data)))))

       '(("echopayload" "foo" "foo"  "foo")
         ("echoevent"   "foo" event  "foo")
         ("error"       "foo" :none  string))))))
