;;;; connector-class.lisp --- Unit tests for the connector-class class.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

(deftestsuite connector-class-root (transport-root)
  ()
  (:documentation
   "Test suite for the `connector-class' class."))

(defgeneric mock-connector-class (which))

(defun call-with-mock-connector-class (thunk which)
  (let+ (((&values superclasses slots options)
          (mock-connector-class which)))
    (eval
     `(defclass ,which ,superclasses
        ,slots
        (:metaclass connector-class)
        ,@options))
    (unwind-protect
         (funcall thunk (find-class which))
      (setf (find-class which) nil))))

(defmacro with-mock-connector-class ((which class-var) &body body)
  (let+ (((&values name ignored?) (if class-var
                                      (values class-var nil)
                                      (values (gensym)  t))))
    `(call-with-mock-connector-class
      (lambda (,name)
        ,@(when ignored? `((declare (ignore ,name))))
        ,@body)
      ',which)))

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

(defmethod mock-connector-class ((which (eql 'foo)))
  (values '()
          '((an-option :initarg  :an-option
                       :type     boolean
                       :initform t
                       :documentation
                       "doc"))
          '((:transport :mock)
            (:direction :in-push)
            (:options (:an-option &slot)))))

(addtest (connector-class-root
          :documentation
          "Test constructing an instance of `connector-class'.")
  construction/valid

  (register-transport
   :mock
   :schemas   :whoop
   :wire-type 'string)

  (with-mock-connector-class (foo class)
    (ensure-same (transport-schemas class)   '(:whoop))
    (ensure-same (transport-wire-type class) 'string)
    (ensure-same (connector-direction class) :in-push)
    (ensure-same (connector-options class)   '((:an-option boolean
                                                :default     t
                                                :description "doc")))))

(defmethod mock-connector-class ((which (eql 'sub)))
  (values '(foo)
          '((another-option :initarg :another-option
                            :type    integer))
          '((:options (:another-option &slot)))))

(addtest (connector-class-root
          :documentation
          "Test constructing an instance of `connector-class'.")
  inheritance

  (register-transport
   :mock
   :schemas   :whoop
   :wire-type 'string)

  (with-mock-connector-class (foo nil)
    (with-mock-connector-class (sub class)
      (ensure-same (transport-schemas class)   '(:whoop))
      (ensure-same (transport-wire-type class) 'string)
      (ensure-same (connector-direction class) :in-push)
      (ensure-same (connector-options class)   '((:another-option integer)
                                                 (:an-option boolean
                                                  :default     t
                                                  :description "doc"))))))
