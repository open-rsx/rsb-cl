;;;; connector-class.lisp --- Unit tests for the connector-class class.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

(def-suite connector-class-root
  :in transport-root
  :description
  "Test suite for the `connector-class' class.")
(in-suite connector-class-root)

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

(test construction/invalid
  "Test error behavior for an invalid constructing an instance of
   `connector-class'."

  ;; We request the option no-such-option to be constructed from the
  ;; slot of the same name. Since there is no such slot, an error has
  ;; to be signaled.
  (eval `(defclass bar ()
           ()
           (:metaclass connector-class)
           (:options
            (:no-such-option &slot))))
  (unwind-protect
       (signals error
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

(test construction/valid
  "Test constructing an instance of `connector-class'."

  (register-transport
   :mock
   :schemas   :whoop
   :wire-type 'string)

  (let ((class (find-class 'foo)))
    (is (equal '(:whoop) (transport-schemas class)))
    (is (eq    'string   (transport-wire-type class)))
    (is (eq    :in-push  (connector-direction class)))
    (is (equal '((:an-option boolean
                  :default     t
                  :description "doc"))
               (connector-options class)))))

(defmethod mock-connector-class ((which (eql 'sub)))
  (values '(foo)
          '((another-option :initarg :another-option
                            :type    integer))
          '((:options (:another-option &slot)))))

(test inheritance
  "Test constructing an instance of `connector-class'."

  (register-transport
   :mock
   :schemas   :whoop
   :wire-type 'string)

  (with-mock-connector-class (foo nil)
    (with-mock-connector-class (sub class)
      (is (equal '(:whoop) (transport-schemas class)))
      (is (eq    'string   (transport-wire-type class)))
      (is (eq    :in-push  (connector-direction class)))
      (is (equal '((:another-option integer)
                   (:an-option boolean
                    :default     t
                    :description "doc"))
                 (connector-options class))))))
