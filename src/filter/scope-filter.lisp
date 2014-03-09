;;;; scope-filter.lisp --- A filter that discriminates based on scopes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defmethod find-filter-class ((spec (eql :scope)))
  (find-class 'scope-filter))

(defclass scope-filter (filter-mixin
                        scope-mixin)
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
   "This filter discriminates based on the scopes of events. Depending
    on whether exact matches are requested, event scopes match the
    scope of the filter, either if they are identical or if they are
    identical or sub-scopes."))

(defmethod matches? ((filter scope-filter) (event event))
  "EVENT is matched by comparing its scope to the scope of FILTER."
  (let+ (((&structure-r/o filter- scope exact?) filter))
    (if exact?
        (scope=/no-coerce (event-scope event) scope)
        (sub-scope?/no-coerce (event-scope event) scope))))

(defmethod print-object ((object scope-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~C ~A"
            (if (filter-exact? object) #\= #\<)
            (scope-string (filter-scope object)))))
