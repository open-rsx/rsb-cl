;;;; conversion-mixin.lisp --- A mixin for converter selection.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport)

(defclass conversion-mixin ()
  ((converter :initarg  :converter
              :accessor connector-converter
              :documentation
              "A converter to which the actual conversion work is
delegated."))
  (:default-initargs
   :converter (missing-required-initarg
               'conversion-mixin :converter))
  (:documentation
   "This mixin adds methods on `domain->wire' and `wire->domain' for
the subclass which delegate the conversion tasks to a stored
converter."))

(defmethod domain->wire ((connector     conversion-mixin)
                         (domain-object t))
  "Delegate conversion of DOMAIN-OBJECT to the converter stored in
CONNECTOR."
  (domain->wire (connector-converter connector) domain-object))

(defmethod wire->domain ((connector   conversion-mixin)
                         (wire-data   t)
                         (wire-schema t))
  "Delegate the conversion of WIRE-DATA, WIRE-SCHEMA to the converter
stored in CONNECTOR."
  (wire->domain (connector-converter connector) wire-data wire-schema))

(defmethod print-object ((object conversion-mixin) stream)
  (let+ (((&accessors-r/o (converter connector-converter)) object)
         (sequence? (typep converter 'sequence)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~:[~S~;(~D)~]"
              sequence? (if sequence? (length converter) converter)))))
