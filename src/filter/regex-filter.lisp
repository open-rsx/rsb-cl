;;;; regex-filter.lisp --- Regular expression filter for event payloads.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; `regex-filter' class

(defclass regex-filter (function-caching-mixin
                        funcallable-filter-mixin
                        payload-matching-mixin
                        fallback-policy-mixin
                        print-items:print-items-mixin)
  ((regex           :type     string
                    :reader   filter-regex
                    :writer   (setf filter-%regex)
                    :documentation
                    "Stores regular expression employed by the
                     filter.")
   (case-sensitive? :initarg  :case-sensitive?
                    :reader   filter-case-sensitive?
                    :initform t
                    :documentation
                    "Stores a boolean which controls whether the
                     regular expression is matched in case insensitive
                     mode."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :regex (missing-required-initarg 'regex-filter :regex))
  (:documentation
   "Discriminate events by matching string payloads against a regular
    expression.

    For payloads of other types, the fallback policy is
    applied. Matching is case sensitive unless otherwise requested
    using the :case-sensitive? initarg."))

(service-provider:register-provider/class 'filter :regex
  :class 'regex-filter)

(defmethod shared-initialize :after ((instance   regex-filter)
                                     (slot-names t)
                                     &key
                                     (regex nil regex-supplied?))
  (when regex-supplied?
    (setf (filter-%regex instance) regex)))

(defmethod (setf filter-%regex) :before ((new-value t)
                                         (filter    regex-filter))
  (check-type new-value string "a string"))

(defmethod compute-filter-function ((filter regex-filter) &key next)
  (let+ (((&structure-r/o filter- regex case-sensitive?) filter)
         (scanner (ppcre:create-scanner
                   regex :case-insensitive-mode (not case-sensitive?))))
    (declare (type function scanner))
    (if next
        (locally (declare (type function next))
          (lambda (payload)
            (cond
              ((not (stringp payload))
               (funcall next payload))
              ((ppcre:scan scanner payload)
               t))))
        (lambda (payload)
          (when (stringp payload)
            (when (ppcre:scan scanner payload) t))))))

(defmethod print-items:print-items append ((object regex-filter))
  (let+ (((&structure-r/o filter- regex case-sensitive?) object))
    `((:regex (,regex ,case-sensitive?) "~{~S~:[/i~;~]~}"
              ((:before :fallback-policy))))))
