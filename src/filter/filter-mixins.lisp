;;;; filter-mixins.lisp --- Mixin classes for filters.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; `funcallable-filter-mixin'

(defclass funcallable-filter-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin makes instances of its subclasses funcallable."))

(defmethod initialize-instance :after ((instance funcallable-filter-mixin)
                                       &key)
  (closer-mop:set-funcallable-instance-function
   instance (curry #'matches? instance)))

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

(defmethod matches? ((filter fallback-policy-mixin)
                     (event  t))
  ;; Decide whether EVENT should match FILTER based on FILTER's
  ;; fallback policy. This method is only called, if no more specific
  ;; method on `matches?' made a decision.
  (ecase (filter-fallback-policy filter)
    (:match        t)
    (:do-not-match nil)))

(defmethod print-object ((object fallback-policy-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (filter-fallback-policy object))))

;;; `payload-matching-mixin'

(defclass payload-matching-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin class is intended to be mixed into filter classes that
    discriminate event based on their payload."))

(defmethod rsb.ep:access? ((transform payload-matching-mixin)
                           (part      (eql :data))
                           (mode      (eql :read)))
  t)

(defmethod matches? ((filter payload-matching-mixin) (event event))
  ;; Decide whether EVENT matches FILTER by calling `payload-matches?'
  ;; on the payload of EVENT.
  (case (payload-matches? filter (event-data event))
    ((nil)        nil)
    (:cannot-tell (call-next-method))
    (t            t)))

(defmethod payload-matches? ((filter payload-matching-mixin) (payload t))
  ;; The default behavior is not to decide based on the payload.
  :cannot-tell)
