;;;; force-wire-schema.lisp --- A converter that sets a given wire-schema.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.converter)

(defmethod find-converter-class ((spec (eql :force-wire-schema)))
  (find-class 'force-wire-schema))

(defclass force-wire-schema ()
  ((wire-schema :initarg  :wire-schema
		:type     keyword
		:accessor converter-wire-schema
		:initform :bytes
		:documentation
		"Stores the wire-schema that should be used when
performing domain->wire \"conversions\"."))
  (:documentation
   "Instances of this class do not perform any changes when converting
between wire-data and domain-data but set a given wire-schema when
producing wire-data, wire-schema pairs."))

(defmethod wire->domain? ((converter   force-wire-schema)
			  (wire-data   t)
			  (wire-schema t))
  "The converter can handle arbitrary wire-data."
  (values converter t))

(defmethod domain->wire? ((converter     force-wire-schema)
			  (domain-object t))
  "The converter can handle arbitrary domain objects."
  (let+ (((&accessors-r/o
	   (wire-schema converter-wire-schema)) converter))
    (values converter t wire-schema)))

(defmethod wire->domain ((converter   force-wire-schema)
			 (wire-data   t)
			 (wire-schema t))
  "The wire-data is not modified."
  wire-data)

(defmethod domain->wire ((converter     force-wire-schema)
			 (domain-object t))
  "The domain object is not modified, but the configured wire-schema
is set."
  (let+ (((&accessors-r/o
	   (wire-schema converter-wire-schema)) converter))
    (values domain-object wire-schema)))

(defmethod print-object ((object force-wire-schema) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (converter-wire-schema object))))
