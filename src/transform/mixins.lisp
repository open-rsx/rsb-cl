;;;; mixins.lisp --- Mixin classes for transforms.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform)

;;; `funcallable-transform-mixin'

(defclass funcallable-transform-mixin (standard-object
                                       function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This class is intended to be mixed into transform classes instance
    of which should be funcallable."))

(defmethod initialize-instance :after ((instance funcallable-transform-mixin)
                                       &key)
  (closer-mop:set-funcallable-instance-function
   instance (curry #'transform! instance)))
