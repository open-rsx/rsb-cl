;;;; payload-matching-mixin.lisp --- Mixin for payload-based filtering.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.filter)

(defclass payload-matching-mixin ()
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "This mixin class is intended to be mixed into filter classes that
discriminate event based on their payload."))

(defmethod matches? ((filter payload-matching-mixin) (event event))
  "Decide whether EVENT matches FILTER by calling `payload-matches?'
on the payload of EVENT."
  (case (payload-matches? filter (event-data event))
    ((nil)        nil)
    (:cannot-tell (call-next-method))
    (t            t)))

(defmethod payload-matches? ((filter payload-matching-mixin) (payload t)
                             &key &allow-other-keys)
  "The default behavior is not to decide based on the payload."
  :cannot-tell)
