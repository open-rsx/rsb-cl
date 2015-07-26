;;;; annotating.lisp --- Annotates unconverted wire-data with wire-schema
;;;;
;;;; Copyright (C) 2011, 2012, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

(defmethod find-converter-class ((spec (eql :annotating)))
  (find-class 'annotating))

(defclass annotating ()
  ()
  (:documentation
   "This converter returns the unconverted wire-data annotated with
    the wire-schema."))

#+no (service-provider:register-provider/class
      'converter :annotating :class 'annotating)

(defstruct (annotated
             (:constructor make-annotated (wire-data wire-schema))
             (:predicate nil)
             (:copier nil))
  (wire-data   nil :read-only t)
  (wire-schema nil :read-only t))

(defmethod wire->domain? ((converter   annotating)
                          (wire-data   t)
                          (wire-schema t))
  ;; The converter can handle arbitrary wire-data.
  (values converter 'annotated))

(defmethod domain->wire? ((converter     annotating)
                          (domain-object annotated))
  ;; The converter can handle annotations it previously produced.
  (let+ (((&structure-r/o annotated- wire-data wire-schema) domain-object))
    (values converter (type-of wire-data) wire-schema)))

(defmethod wire->domain ((converter   annotating)
                         (wire-data   t)
                         (wire-schema t))
  ;; The wire-data is not modified, but the wire-schema is attached.
  (make-annotated wire-data wire-schema))

(defmethod domain->wire ((converter     annotating)
                         (domain-object annotated))
  ;; The domain object is not modified, but it is separated from the
  ;; wire-schema.
  (let+ (((&structure-r/o annotated- wire-data wire-schema) domain-object))
    (values wire-data wire-schema)))
