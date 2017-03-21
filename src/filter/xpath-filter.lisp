;;;; xpath-filter.lisp --- XPath-based filtering.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; `xpath-filter' class

(defclass xpath-filter (function-caching-mixin
                        funcallable-filter-mixin
                        payload-matching-mixin
                        fallback-policy-mixin
                        print-items:print-items-mixin)
  ((xpath          :type     xpath::xpath-expr
                   :reader   filter-xpath
                   :writer   (setf filter-%xpath)
                   :documentation
                   "The XPath used by the filter to discriminate events.")
   (namespaces     :initarg  :namespaces
                   :type     (or null (cons (cons string string) list))
                   :reader   filter-namespaces
                   :initform '()
                   :documentation
                   "Stores a mapping of prefixes to namespace URIs as
                    an alist with elements of the form

                      (PREFIX . NAMESPACE-URI)

                    .")
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
                                     (xpath nil xpath-supplied?))
  (when xpath-supplied?
    (setf (filter-%xpath instance) xpath)))

(defmethod (setf filter-%xpath) :before ((new-value t)
                                         (filter    xpath-filter))
  (check-type new-value xpath::xpath-expr
              "an XPath string or an XPath sexp expression")
  (setf (filter-%compiled-xpath filter)
        (let ((xpath::*dynamic-namespaces*
               (append (filter-namespaces filter)
                       xpath::*dynamic-namespaces*)))
          (xpath:compile-xpath new-value))))

(defmethod rsb.ep:access? ((processor xpath-filter)
                           (part      t)
                           (mode      (eql :read)))
  t)

(defmethod compute-filter-function ((filter xpath-filter) &key next)
  (declare (type function next))
  (if next
      (locally (declare (type function next))
        (lambda (payload)
          (case (payload-matches? filter payload)
            (:cannot-tell (funcall next payload))
            ((nil)        nil)
            (t            t))))
      (lambda (payload)
        (case (payload-matches? filter payload)
          ((:cannot-tell nil) nil)
          (t                  t)))))

(defmethod print-items:print-items append ((object xpath-filter))
  `((:xpath ,(filter-xpath object) "~S")))

;;; Utility functions

(defun xpath-result->filter-result (result)
  ;; Return a non-nil if RESULT represents a matching XPath result and
  ;; nil otherwise.
  (typecase result
    (xpath:node-set (not (xpath:node-set-empty-p result)))
    (string         (emptyp result))
    (t              result)))
