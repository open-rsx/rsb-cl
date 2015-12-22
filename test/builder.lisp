;;;; builder.lisp --- Unit tests for builder support.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.builder.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:lift

   #:rsb
   #:rsb.builder)

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

(defun check-un-build-calls (builder atom-type cases &key peek-function)
  (mapc (lambda+ ((object expected-calls))
          (let+ (((&values &ign calls)
                  (record-un-build-calls/peeking
                   #'bp:walk-nodes builder atom-type object
                   :peek-function peek-function)))
            (ensure-same calls expected-calls :test #'equal)))
        cases))

(addtest (rsb-builder-root
          :documentation
          "Smoke test for \"unbuilding\" `scope' instances.")
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
          "Smoke test for the \"unbuilding\" `event' instances.")
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

(defclass mock-payload ()
  ((slot :initarg  :slot
         :type     vector
         :initform #(1 2))))

(addtest (rsb-builder-root
          :documentation
          "Smoke test for \"unbuilding\" `event' instances, switching
           to universal builder for the data relation.")
  event/universal-builder-for-event-data

  (check-un-build-calls
   t '(or number string
          uuid:uuid local-time:timestamp
          puri:uri scope)
   `(,(let* ((scope  (make-scope "/foo"))
             (data   (make-instance 'mock-payload))
             (create (local-time:now))
             (event  (make-event scope data
                                 :method            :|request|
                                 :timestamps        (list :create create)
                                 :create-timestamp? nil
                                 :foo               "bar")))
        `(,event ((:peek  nil        ()             ,event)
                  (:visit nil        ()             ,event rsb:event
                          ((:meta-data . (:map . :key))
                           (:timestamp . (:map . :key))
                           (:cause     . *)
                           (:data      . 1))
                          (:scope ,scope :method :|request|))
                  (:peek  :meta-data (:key :foo)    "bar")
                  (:peek  :timestamp (:key :create) ,create)
                  (:peek  :data      ()             ,data)
                  (:visit :data      ()             ,data mock-payload
                          ((:slot . *))
                          ())
                  (:peek  :slot      ()             1)
                  (:peek  :slot      ()             2)))))
   :peek-function (universal-builder-for-event-data)))
