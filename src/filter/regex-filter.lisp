;;;; regex-filter.lisp --- Regular expression filter for event payloads.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; Protocol

(defgeneric compile-regex (filter regex
                           &key
                           case-sensitive?)
  (:documentation
   "Compile REGEX and store the resulting compiled scanner in
    FILTER.

    CASE-SENSITIVE? controls whether the created scanner performs
    case-sensitive matching."))

;;; `regex-filter' class

(defclass regex-filter (funcallable-filter-mixin
                        payload-matching-mixin
                        fallback-policy-mixin
                        print-items:print-items-mixin)
  ((regex           :type     string
                    :accessor filter-regex
                    :documentation
                    "Stores regular expression employed by the
                     filter.")
   (scanner         :reader   filter-scanner
                    :writer   (setf filter-%scanner)
                    :documentation
                    "Stores the compiled scanner corresponding to the
                     regular expression of the filter.")
   (case-sensitive? :initarg  :case-sensitive?
                    :accessor filter-case-sensitive?
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
                                     regex)
  (check-type regex string "a string")

  (setf (filter-regex instance) regex))

(defmethod (setf filter-regex) :before ((new-value string)
                                        (filter    regex-filter))
  ;; Create a new scanner for FILTER for the regex NEW-VALUE.
  (compile-regex filter new-value
                 :case-sensitive? (filter-case-sensitive? filter)))

(defmethod (setf filter-case-sensitive?) :before ((new-value t)
                                                  (filter    regex-filter))
  ;; Create a new scanner for FILTER for the regex NEW-VALUE.
  (compile-regex filter (filter-regex filter)
                 :case-sensitive? new-value))

(defmethod compile-regex ((filter regex-filter) (regex string)
                          &key
                          case-sensitive?)
  (setf (filter-%scanner filter)
        (ppcre:create-scanner
         regex :case-insensitive-mode (not case-sensitive?))))

(defmethod payload-matches? ((filter regex-filter) (payload string))
  (ppcre:scan (filter-scanner filter) payload))

(defmethod print-items:print-items append ((object regex-filter))
  (let+ (((&structure-r/o filter- regex case-sensitive?) object))
    `((:regex (,regex ,case-sensitive?) "~{~S~:[/i~;~]~}"
              ((:before :fallback-policy))))))
