;;;; mixins.lisp --- Unit tests for mixins classes.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.test)

(deftestsuite uuid-mixin-root (root)
  ()
  (:documentation
   "Test suite for the `uuid-mixin' class."))

(addtest (uuid-mixin-root
          :documentation
          "Test constructing `uuid-mixin' instances for different
values of `*id-random-state*'.")
  random-state

  ;; Binding `*id-random-state*' to identical values has to result in
  ;; `uuid-mixin' instances with identical ids.
  (let+ ((state (make-random-state))
         ((&values a b)
          (values
           (let ((*id-random-state* (make-random-state state)))
             (make-instance 'uuid-mixin))
           (let ((*id-random-state* (make-random-state state)))
             (make-instance 'uuid-mixin)))))
    (ensure-same (slot-value a 'rsb::id) (slot-value b 'rsb::id)
                 :test #'uuid:uuid=))

  ;; Using the current value of `*id-random-state*' multiple times has
  ;; to result in different ids.
  (ensure-different
   (slot-value (make-instance 'uuid-mixin) 'rsb::id)
   (slot-value (make-instance 'uuid-mixin) 'rsb::id)
   :test #'uuid:uuid=))
