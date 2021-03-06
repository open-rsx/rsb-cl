;;;; filter-mixins.lisp --- Mixin classes for filters.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; `function-caching-mixin'

(defclass function-caching-mixin ()
  ((function :type     function
             :reader   filter-function
             :writer   (setf filter-%function)))
  (:documentation
   "This mixin caches a computed filter function."))

(defmethod shared-initialize :around ((instance   function-caching-mixin)
                                      (slot-names t)
                                      &key)
  (call-next-method)
  (update-filter-function instance))

(defmethod matches? ((filter function-caching-mixin)
                     (event  t))
  (funcall (the function (filter-function filter)) event))

(defun update-filter-function (filter)
  (setf (filter-%function filter) (compute-filter-function filter)))

;;; `funcallable-filter-mixin'

(defclass funcallable-filter-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin makes instances of its subclasses funcallable.

    It can be used in conjunction with `function-caching-mixin'."))

(defmethod (setf filter-%function) :after ((new-value function)
                                           (instance  funcallable-filter-mixin))
  (closer-mop:set-funcallable-instance-function instance new-value))

;;; `fallback-policy-mixin'

(defclass fallback-policy-mixin ()
  ((fallback-policy :initarg  :always
                    :initarg  :fallback-policy
                    :type     fallback-policy
                    :accessor filter-fallback-policy
                    :initform :match
                    :documentation
                    "The value of this slots determines the behavior
                     of the filter in case it primary discrimination
                     mechanism is not applicable to an event."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin class is intended to be mixed into filter classes that
    cannot process all events using their primary discrimination
    method and thus need a fallback policy."))

(service-provider:register-provider/class 'filter :constant
  :class 'fallback-policy-mixin)

(defmethod compute-filter-function :around ((filter fallback-policy-mixin)
                                            &key next)
  (declare (ignore next))
  ;; Decide whether EVENT should match FILTER based on FILTER's
  ;; fallback policy. This method is only called, if no more specific
  ;; method on `matches?' made a decision.
  (let ((fallback-result (case (filter-fallback-policy filter)
                           (:match        t)
                           (:do-not-match nil))))
    (call-next-method filter :next (lambda (event)
                                     (declare (ignore event))
                                     fallback-result))))

(defmethod matches? ((filter fallback-policy-mixin)
                     (event  t))
  ;; Decide whether EVENT should match FILTER based on FILTER's
  ;; fallback policy. This method is only called, if no more specific
  ;; method on `matches?' made a decision.
  (ecase (filter-fallback-policy filter)
    (:match        t)
    (:do-not-match nil)))

(defmethod print-items:print-items append ((object fallback-policy-mixin))
  `((:fallback-policy ,(filter-fallback-policy object) " or ~A")))

;;; `payload-matching-mixin'

(defclass payload-matching-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin class is intended to be mixed into filter classes that
    discriminate event based on their payload."))

(defmethod rsb.ep:access? ((processor payload-matching-mixin)
                           (part      (eql :data))
                           (mode      (eql :read)))
  t)

(defmethod compute-filter-function :around ((filter payload-matching-mixin)
                                            &key next)
  (let ((function (call-next-method filter :next next)))
    (declare (type function function))
    (lambda (event)
      (funcall function (event-data event)))))

(defmethod matches? ((filter payload-matching-mixin) (event event))
  ;; Decide whether EVENT matches FILTER by calling `payload-matches?'
  ;; on the payload of EVENT.
  (case (payload-matches? filter (event-data event))
    (:cannot-tell (call-next-method))
    ((nil)        nil)
    (t            t)))

(defmethod payload-matches? ((filter payload-matching-mixin) (payload t))
  ;; The default behavior is not to decide based on the payload.
  :cannot-tell)

;;; Class `composite-filter-mixin'

(defclass composite-filter-mixin ()
  ((children :type     list
             :accessor filter-children
             :initform '()
             :documentation
             "A list of subordinate filters."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of subclasses of this class implement complex filtering
    behavior by combining decisions of a set of subordinate
    filters. On rare occasions is it useful to make instances of this
    class itself rather than subclasses."))

(defmethod shared-initialize :after ((instance   composite-filter-mixin)
                                     (slot-names t)
                                     &key
                                     (children nil children-supplied?))
  (when children-supplied?
    (setf (filter-children instance) children)))

(defmethod rsb.ep:access? ((processor composite-filter-mixin)
                           (part      t)
                           (mode      t))
  (rsb.ep:access? (filter-children processor) part mode))

(defmethod (setf filter-children) :after ((new-value t)
                                          (filter    composite-filter-mixin))
  (update-filter-function filter))

(defmethod print-items:print-items append ((object composite-filter-mixin))
  `((:child-count ,(length (filter-children object)) "(~D)")))
