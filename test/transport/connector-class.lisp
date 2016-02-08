;;;; connector-class.lisp --- Unit tests for the connector-class class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

(deftestsuite connector-class-root (transport-root)
  ()
  (:documentation
   "Test suite for the `connector-class' class."))

(addtest (connector-class-root
          :documentation
          "Test constructing an instance of `connector-class'.")
  construction/valid

  (register-transport
   :mock
   :wire-type 'string)

  (eval
   '(defclass foo ()
      ((an-option :initarg  :an-option
                  :type     boolean
                  :initform t
                  :documentation
                  "doc"))
      (:metaclass connector-class)
      (:transport :mock)
      (:direction :in-push)
      (:wire-type string)
      (:schemas   :whoop)
      (:options
       (:an-option &slot))))

  (let ((class (find-class 'foo)))
    (ensure-same (connector-direction class) :in-push)
    (ensure-same (connector-wire-type class) 'string)
    (ensure-same (connector-schemas class)   '(:whoop))
    (ensure-same (connector-options class)   '((:an-option boolean
                                                :default     t
                                                :description "doc")))))

(addtest (connector-class-root
          :documentation
          "Test error behavior for an invalid constructing an instance
           of `connector-class'.")
  construction/invalid

  ;; We request the option no-such-option to be constructed from the
  ;; slot of the same name. Since there is no such slot, an error has
  ;; to be signaled.
  (eval `(defclass bar ()
           ()
           (:metaclass connector-class)
           (:options
            (:no-such-option &slot))))
  (unwind-protect
       (ensure-condition 'error
         (closer-mop:finalize-inheritance (find-class 'bar)))
    (eval `(defclass bar ()
             ()
             (:metaclass connector-class)
             (:options)))
    (setf (find-class 'bar) nil)))
