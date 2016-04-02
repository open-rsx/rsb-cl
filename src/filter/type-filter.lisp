;;;; type-filter.lisp --- A filter that discriminates event based on their type.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass type-filter (function-caching-mixin
                       funcallable-filter-mixin
                       payload-matching-mixin
                       print-items:print-items-mixin)
  ((type :initarg  :type
         :type     (or list symbol)
         :reader   filter-type
         :documentation
         "The type of matching events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :type (missing-required-initarg 'type-filter :type))
  (:documentation
   "Discriminate based on the type of event payloads."))

(service-provider:register-provider/class 'filter :type
  :class 'type-filter)

(defmethod compute-filter-function ((filter type-filter) &key next)
  (declare (ignore next))
  (let+ ((type (filter-type filter))
         ((&values function &ign failure?)
          (block compile
            (handler-bind (((and warning (not style-warning))
                             (lambda (condition)
                               (return-from compile (values nil condition t)))))
              (let ((*error-output* (make-broadcast-stream)))
                (with-compilation-unit (:override t)
                  (compile nil `(lambda (payload)
                                  (typep payload ',type)))))))))
    (when failure?
      (error 'simple-type-error
             :datum            type
             :type             '(or cons symbol)
             :format-control   "~@<~S is not a valid type specifier.~@:>"
             :format-arguments (list type)))
    function))

(defmethod print-items:print-items append ((object type-filter))
  `((:type ,(filter-type object))))
