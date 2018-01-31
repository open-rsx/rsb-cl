;;;; assembly-mixin.lisp --- Mixins related to assembling notification fragments.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

(defclass assembly-mixin ()
  ((assembly-pool :initarg  :assembly-pool
                  :type     assembly-pool
                  :reader   connector-assembly-pool
                  :documentation
                  "Stores the pool used to assemble notification
                   fragments."))
  (:metaclass connector-class)
  (:options
   (:age-limit positive-real
    :default 10
    :description
    #.(format nil "The amount of time after which incomplete ~
       assemblies are pruned. Supplying this option only makes sense ~
       in conjunction with an unreliable communication mode since ~
       incomplete assemblies are never pruned in reliable ~
       communication modes.")))
  (:documentation
   "Adds an assembly pool and a `notification->event' method.

    These can be used to assemble incoming notifications (which may
    originate from fragmented events) into events."))

(defmethod initialize-instance :after ((instance assembly-mixin)
                                       &key
                                       assembly-pool
                                       age-limit)
  (unless assembly-pool
    (setf (slot-value instance 'assembly-pool)
          (if age-limit
              (make-instance 'pruning-assembly-pool
                             :age-limit age-limit)
              (make-instance 'assembly-pool)))))
