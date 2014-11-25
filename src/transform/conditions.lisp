;;;; conditions.lisp --- Conditions used by the transform module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform)

;;; Transform application conditions

(define-condition transform-error (rsb-error
                                   chainable-condition)
  ((transform :initarg  :transform
              :reader   transform-error-transform
              :documentation
              "Stores the failed transform.")
   (object    :initarg  :object
              :reader   transform-error-object
              :documentation
              "Stores the object for which the transform failed."))
  (:default-initargs
   :transform (missing-required-initarg 'transform-error :transform)
   :object    (missing-required-initarg 'transform-error :object))
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not apply transform ~A to ~
                     ~A~/more-conditions:maybe-print-cause/~@:>"
             (transform-error-transform condition)
             (transform-error-object    condition)
             condition)))
  (:documentation
   "This error is signaled when a transform fails."))

;;; Transform creation conditions

(define-condition transform-creation-error (rsb-error
                                            chainable-condition)
  ((spec :initarg  :spec
         :reader   transform-creation-error-spec
         :documentation
         "The transform specification for which the attempt to
          construct a transform instance failed."))
  (:default-initargs
   :spec (missing-required-initarg 'transform-cration-error :spec))
  (:report
   (lambda (condition stream)
     (let+ (((key &rest initargs) (transform-creation-error-spec condition)))
       (format stream "~@<Failed to construct ~A transform ~:[~
                         without options~
                       ~:;~
                         ~:*with options ~
                         ~@:_~@:_~
                         ~2@T~<~@;~{~16S~^ ~S~^~@:_~}~:>~
                         ~@:_~@:_~
                       ~].~/more-conditions:maybe-print-cause/~@:>"
               key (when initargs (list initargs)) condition))))
  (:documentation
   "This error is signaled when an attempt to construct a transform
    instance based on a transform specification fails."))
