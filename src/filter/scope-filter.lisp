;;;; scope-filter.lisp --- A filter that discriminates based on scopes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defclass scope-filter (funcallable-filter-mixin
                        scope-mixin
                        print-items:print-items-mixin)
  ((rsb::scope :accessor filter-scope
               :documentation
               "A superscope of the scopes of matching events.")
   (exact?     :initarg  :exact?
               :type     boolean
               :accessor filter-exact?
               :initform nil
               :documentation
               "Do events have to have exactly the specified scope or
                are sub-scopes permitted?"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Discriminate based on the scopes of events.

    Depending on whether exact matches are requested, event scopes
    match the scope of the filter, either if they are identical or if
    they are identical or sub-scopes."))

(service-provider:register-provider/class 'filter :scope
  :class 'scope-filter)

(defmethod rsb.ep:access? ((processor scope-filter)
                           (part      (eql :scope))
                           (mode      (eql :read)))
  t)

(defmethod matches? ((filter scope-filter) (event event))
  ;; EVENT is matched by comparing its scope to the scope of FILTER.
  (let+ (((&structure-r/o filter- scope exact?) filter))
    (if exact?
        (scope=/no-coerce (event-scope event) scope)
        (sub-scope?/no-coerce (event-scope event) scope))))

(defmethod print-items:print-items append ((object scope-filter))
  `((:relation ,(if (filter-exact? object) #\= #\<)  "~C")
    (:scope    ,(scope-string (filter-scope object)) " ~A"
               ((:after :relation)))))
