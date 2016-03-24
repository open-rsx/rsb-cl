;;;; caching.lisp --- Unit test for the caching-converter class.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter.test)

(deftestsuite caching-root (converter-root)
  ()
  (:documentation
   "Unit tests for the `caching-converter' class."))

(define-basic-converter-test-cases
    (:caching
     :make-converter (make-instance 'rsb.converter:caching-converter
                                    :target :fundamental-void)
     :simple?        nil)
  `((,(octetify #())  :void ,+no-value+)
    (,(octetify #(1)) :void :not-applicable)
    (:not-applicable  :void t)
    (:not-applicable  :void ,(octetify #()))
    (:not-applicable  :void ,(octetify #(1)))))
