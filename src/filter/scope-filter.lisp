;;;; scope-filter.lisp --- A filter that discriminates based on scopes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

(defmethod find-filter-class ((spec (eql :scope)))
  (find-class 'scope-filter))

(defclass scope-filter (filter-mixin
                        scope-mixin)
  ((rsb::scope :accessor filter-scope
               :documentation
               "A superscope of the scopes of matching events."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This filter discriminates based on the scopes of events. Event
scopes match the scope of the filter, if they are identical or
sub-scopes."))

(defmethod matches? ((filter scope-filter) (event event))
  "EVENT is matched by comparing its scope to the scope of FILTER."
  (sub-scope? (event-scope event) (filter-scope filter)))

(defmethod print-object ((object scope-filter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (scope-string (filter-scope object)))))
