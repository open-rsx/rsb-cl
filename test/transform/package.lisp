;;;; package.lisp --- Package definition for unit tests of the transform module.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transform.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:fiveam

   #:rsb
   #:rsb.transform

   #:rsb.test)

  (:documentation
   "This package contains unit tests for the transform module"))

(cl:in-package #:rsb.transform.test)

(def-suite* rsb.transform-root
  :in root
  :description
  "Root unit test suite for the transform module.")

(defun call-with-transform-checking-thunk
    (thunk transform-spec event-spec)
  "Call THUNK with a function as the sole argument, that
   1. Constructs a transform according to TRANSFORM-SPEC
   2. Constructs an `access-checking-event' according to EVENT-SPEC
   3. Transforms the event using the transform
   4. Returns the transformed event"
  (let+ (((&flet check (funcall?)
            (let+ ((transform (apply #'make-transform
                                     (ensure-list transform-spec)))
                   (event     (apply #'make-access-checking-event-for-processor
                                     transform event-spec))
                   ((&flet do-it ()
                      (with-access-checking ()
                        (if funcall?
                            (funcall transform event)
                            (transform! transform event))))))
              (funcall thunk #'do-it)))))
    (check nil)
    (check t)))
