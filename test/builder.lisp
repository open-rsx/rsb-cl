;;;; builder.lisp --- Unit tests for builder support.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.builder.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:lift

   #:rsb)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:architecture.builder-protocol.test
   #:record-un-build-calls/peeking)

  (:export
   #:rsb-builder-root)

  (:documentation
   "This package contains test for the builder module."))

(cl:in-package #:rsb.builder.test)

(deftestsuite rsb-builder-root (root)
  ()
  (:documentation
   "Unit tests for builder support."))

(defun check-un-build-calls (builder atom-type cases)
  (mapc (lambda+ ((object expected-calls))
          (let ((calls (record-un-build-calls/peeking
                        builder atom-type object)))
            (ensure-same calls expected-calls :test #'equal)))
        cases))

(addtest (rsb-builder-root
          :documentation
          "Smoke test for the `transport-options'.")
  scope/smoke

  (check-un-build-calls
   t 'string
   `(,(let ((scope (make-scope "/")))
        `(,scope ((:peek  nil        () ,scope)
                  (:visit nil        () ,scope rsb:scope ((:component . *)) ()))))

     ,(let ((scope (make-scope "/foo/bar")))
        `(,scope ((:peek  nil        () ,scope)
                  (:visit nil        () ,scope rsb:scope ((:component . *)) ())
                  (:peek  :component () "foo")
                  (:peek  :component () "bar")))))))

(addtest (rsb-builder-root
          :documentation
          "Smoke test for the `transport-options'.")
  event/smoke

  (check-un-build-calls
   t '(or number string
          uuid:uuid local-time:timestamp
          puri:uri scope)
   `(,(let* ((scope  (make-scope "/foo"))
             (create (local-time:now))
             (event  (make-event scope "baz"
                                 :method            :|request|
                                 :timestamps        (list :create create)
                                 :create-timestamp? nil
                                 :foo               "bar")))
        `(,event ((:peek  nil        () ,event)
                  (:visit nil        () ,event rsb:event
                          ((:meta-data . (:map . :key))
                           (:timestamp . (:map . :key))
                           (:cause     . *)
                           (:data      . 1))
                          (:scope ,scope :method :|request|))
                  (:peek  :meta-data (:key :foo)    "bar")
                  (:peek  :timestamp (:key :create) ,create)
                  (:peek  :data      ()             "baz")))))))
