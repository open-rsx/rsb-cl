;;;; annotating.lisp --- Unit test for the annotating converter class.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter.test)

(def-suite* annotating-root
  :in converter-root
  :description
  "Unit tests for the `annotating' converter class.")

(define-basic-converter-test-cases (:annotating
                                    :make-converter (make-instance 'rsb.converter::annotating))
    `((:some-wire-data :some-wire-schema
       ,(rsb.converter::make-annotated :some-wire-data :some-wire-schema))
      (:not-applicable :some-wire-schema :foo)))
