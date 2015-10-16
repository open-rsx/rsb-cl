;;;; xpath-filter.lisp --- XPath-based filtering.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; Protocol

(defgeneric compile-xpath (filter xpath)
  (:documentation
   "Produce and return a compiled representation of XPATH that can be
    used with FILTER."))

;;; `xpath-filter' class

(defclass xpath-filter (funcallable-filter-mixin
                        payload-matching-mixin
                        fallback-policy-mixin)
  ((xpath          :type     string
                   :accessor filter-xpath
                   :documentation
                   "The XPath used by the filter to discriminate
                    events.")
   (compiled-xpath :type     function
                   :reader   filter-compiled-xpath
                   :writer   (setf filter-%compiled-xpath)
                   :documentation
                   "A compiled version of the XPath of the filter."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :xpath (missing-required-initarg 'xpath-filter :xpath))
  (:documentation
   "Discriminate events based on XPath expressions.

    It is applicable to payloads for which an implementation of the
    XPath interface is available. Examples include strings (via XML
    parsing), XML DOM objects and protocol buffer messages."))

(service-provider:register-provider/class 'filter :xpath
  :class 'xpath-filter)

(defmethod shared-initialize :after ((instance   xpath-filter)
                                     (slot-names t)
                                     &key
                                     xpath)
  (check-type xpath xpath::xpath-expr
              "an XPath string or an XPath sexp expression")

  (setf (filter-xpath instance) xpath))

(defmethod (setf filter-xpath) :before ((new-value string)
                                        (filter    xpath-filter))
  ;; Compile the XPath.
  (setf (filter-%compiled-xpath filter)
        (compile-xpath filter new-value)))

(defmethod compile-xpath ((filter xpath-filter)
                          (xpath  string))
  (xpath:compile-xpath xpath))

(defmethod print-object ((object xpath-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (filter-xpath object))))

;;; Utility functions

(defun xpath-result->filter-result (result)
  "Return a non-nil if RESULT represents a matching XPath result and
   nil otherwise."
  (typecase result
    (xpath:node-set (not (xpath:node-set-empty-p result)))
    (string         (emptyp result))
    (t              result)))
