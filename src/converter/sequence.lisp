;;;; sequence.lisp --- Sequences of alternative converters.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

;;; Sequence "converter"
;;;
;;; Sequences are interpreted as a prioritized list of alternative
;;; converters. That is, the first converter that is able to perform a
;;; requested conversion is selected and used.

(defmethod wire->domain? ((converter   sequence)
                          (wire-data   t)
                          (wire-schema t))
  "Check whether any child can handle the requested conversion."
  (iter (for child in converter)
        (let+ (((&values use? domain-type)
                (wire->domain? child wire-data wire-schema)))
          (when use?
            (return (values use? domain-type))))))

(defmethod domain->wire? ((converter     sequence)
                          (domain-object t))
  "Check whether any child can handle the requested conversion."
  (iter (for child in converter)
        (let+ (((&values use? wire-type wire-schema)
                (domain->wire? child domain-object)))
          (when use?
            (return (values use? wire-type wire-schema))))))

(defmethod wire->domain ((converter   sequence)
                         (wire-data   t)
                         (wire-schema t))
  (if-let ((child (wire->domain? converter wire-data wire-schema)))
    (wire->domain child wire-data wire-schema)
    (error "~@<None of the available converters could handle the ~
            wire-data. Tried ~{~A~^, ~}~@:>"
           converter)))

(defmethod domain->wire ((converter     sequence)
                         (domain-object t))
  (if-let ((child (domain->wire? converter domain-object)))
    (domain->wire child domain-object)
    (error "~@<None of the available converters could handle the ~
            domain-object. Tried ~{~A~^, ~}~@:>"
           converter)))
