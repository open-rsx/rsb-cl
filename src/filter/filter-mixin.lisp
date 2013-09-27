;;;; filter-mixin.lisp --- Mixin class that makes filter instances funcallable.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.filter)

(defclass filter-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin makes instances of its subclasses funcallable."))

(defmethod initialize-instance :after ((instance filter-mixin)
				       &key)
  (closer-mop:set-funcallable-instance-function
   instance
   #'(lambda (event)
       (matches? instance event))))
