;;;; processor-mixins.lisp --- Mixin classes for processor classes.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.event-processing)

;;; `transform-mixin' class

(defclass transform-mixin ()
  ((transform :initarg  :transform
              :reader   processor-transform
              :documentation
              "Stores a transform (in the sense of being usable with
`transform!') that should be applied to all handled data."))
  (:default-initargs
   :transform (missing-required-initarg 'transform-mixin :transform))
  (:documentation
   "This mixin class adds to processor classes the ability to apply a
transform (in the sense of being usable with `transform!') to all
handled data."))

(defmethod handle :around ((sink transform-mixin) (data t))
  (call-next-method sink (transform! (processor-transform sink) data)))
