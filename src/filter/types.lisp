;;;; types.lisp --- Types used in the filter module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.filter)

;;; Event-ID related types

(deftype uuid-designator ()
  `(or uuid:uuid string (nibbles:octet-vector 16)))

(deftype cause-pattern/origin ()
  `(cons uuid:uuid (eql *)))

(deftype cause-pattern/sequence-number ()
  `(cons (eql *) sequence-number))

(deftype cause-pattern ()
  `(or (eql t)
       event-id uuid:uuid
       cause-pattern/origin
       cause-pattern/sequence-number))

;;; Filter behavior-related types

(deftype fallback-policy ()
  "Designators for fallback filter behaviors."
  '(member :match :do-not-match))
