;;;; type-filter.lisp --- A filter that discriminates event based on their type.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass type-filter (funcallable-filter-mixin
                       payload-matching-mixin)
  ((type      :type     (or list symbol)
              :accessor filter-type
              :documentation
              "The type of matching events.")
   (predicate :type     (or null function)
              :accessor filter-%predicate
              :initform nil
              :documentation
              "Stores a compiled predicate implementing the filter or
               nil if none is available."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :type (missing-required-initarg 'type-filter :type))
  (:documentation
   "Discriminate based on the type of event payloads."))

(service-provider:register-provider/class 'filter :type
  :class 'type-filter)

(defmethod shared-initialize :after ((instance   type-filter)
                                     (slot-names t)
                                     &key
                                     (type nil type-supplied?))
  (when type-supplied?
    (setf (filter-type instance) type)))

(defmethod (setf filter-type) :before ((new-value t)
                                       (filter    type-filter))
  (let+ (((&values function &ign failure?)
          (block compile
            (handler-bind (((and warning (not style-warning))
                            (lambda (condition)
                              (return-from compile (values nil condition t)))))
              (let ((*error-output* (make-broadcast-stream)))
                (with-compilation-unit (:override t)
                  (compile nil `(lambda (payload) (typep payload ',new-value)))))))))
    (when failure?
      (error 'simple-type-error
             :datum            new-value
             :type             '(or cons symbol)
             :format-control   "~@<~S is not a valid type specifier.~@:>"
             :format-arguments (list new-value)))
    (setf (filter-%predicate filter) (unless failure? function))))

(defmethod rsb.ep:access? ((transform type-filter)
                           (part      (eql :data))
                           (mode      (eql :read)))
  t)

(defmethod payload-matches? ((filter type-filter) (payload t))
  (if-let ((predicate (filter-%predicate filter)))
    (locally (declare (type function predicate))
      (funcall predicate payload))
    (typep payload (filter-type filter))))

(defmethod print-object ((object type-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-type object))))
