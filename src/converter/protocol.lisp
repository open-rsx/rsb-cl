;;;; protocol.lisp --- Wire <-> domain conversion protocol.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

;;; Converter protocol

(defgeneric wire->domain (converter wire-data wire-schema)
  (:documentation
   "Decode WIRE-DATA into a Lisp object using an interpretation
according to WIRE-SCHEMA and CONVERTER. Return the decoded Lisp
object.

Example:
RSB.CONVERTER> (wire->domain :fundamental-string #(102 111 111) :string)
=> \"foo\""))

(defgeneric domain->wire (converter domain-object)
  (:documentation
   "Encode the Lisp object DOMAIN-OBJECT into the wire representation
associated to CONVERTER. Return two values: the constructed wire
representation and the wire-schema.

Example:
RSB.CONVERTER> (domain->wire :fundamental-string \"foo\")
=> #(102 111 111) :string"))

;;; Converter info protocol

(defgeneric wire->domain? (converter wire-data wire-schema)
  (:documentation
   "Return non-nil if CONVERTER can be used to convert WIRE-DATA into
a Lisp object using the interpretation designated by WIRE-SCHEMA. If
such a conversion is possible, return two values: CONVERTER and the
type of the Lisp object that the conversion would produce.

Example:
RSB.CONVERTER> (wire->domain? :fundamental-string #(102 111 111) :string)
=> :fundamental-string 'string"))

(defgeneric domain->wire? (converter domain-object)
  (:documentation
   "Return non-nil if CONVERTER can be used to convert DOMAIN-OBJECT
to its wire-type. If such a conversion is possible, return three
values: CONVERTER, the wire-type and the wire-schema the conversion
would produce.

Example:
RSB.CONVERTER> (domain->wire? :fundamental-string \"foo\")
=> :fundamental-string 'octet-vector :string"))

;;; Default behavior

(defmethod no-applicable-method ((function (eql (fdefinition 'wire->domain?)))
                                 &rest args)
  "If there is no method on `wire->domain?' for a given combination of
converter, wire-data and wire-schema, the converter cannot handle the
data."
  (declare (ignore args))
  nil)

(defmethod no-applicable-method ((function (eql (fdefinition 'domain->wire?)))
                                 &rest args)
  "If there is no method on `domain->wire?' for a given pair of
converter and data, the converter cannot handle the data."
  (declare (ignore args))
  nil)

(defmethod wire->domain :around ((converter   t)
                                 (wire-data   t)
                                 (wire-schema t))
  "Install \"retry\" and \"use-value\" restarts around the call to the
next `wire->domain' method."
  (iter
    (restart-case
        (with-condition-translation
            (((error wire->domain-conversion-error)
              :wire-schema wire-schema
              :encoded     wire-data
              :domain-type :undetermined))
          (return (call-next-method)))
      (retry ()
        :report (lambda (stream)
                  (format stream "~@<Retry converting ~S (in ~S ~
                                  schema) using converter ~A.~@:>"
                          wire-data wire-schema converter)) nil)
      (use-value (value)
        :report      (lambda (stream)
                       (format stream "~@<Supply a replacement value ~
                                       to use instead of converting ~
                                       ~S (in ~S schema) using ~
                                       converter ~A.~@:>"
                               wire-data wire-schema converter))
        :interactive (lambda ()
                       (format *query-io* "Enter replacement value (evaluated): ")
                       (list (read *query-io*)))
        (return value)))))

(defmethod domain->wire :around ((converter     t)
                                 (domain-object t))
  "Install \"retry\" and \"use-value\" restarts around the call to the
next `domain->wire' method."
  (iter
    (restart-case
        (with-condition-translation
            (((error domain->wire-conversion-error)
              :wire-schema   :undetermined
              :domain-object domain-object
              :wire-type     :undetermined))
          (return (call-next-method)))
      (retry ()
        :report (lambda (stream)
                  (format stream "~@<Retry converting ~A using ~
                                  converter ~A.~@:>"
                          domain-object converter))
        nil)
      (use-value (value)
        :report      (lambda (stream)
                       (format stream "~@<Supply a replacement value ~
                                       to use instead of converting ~A ~
                                       using converter ~A.~@:>"
                               domain-object converter))
        :interactive (lambda ()
                       (format *query-io* "Enter replacement value (evaluated): ")
                       (list (read *query-io*)))
        (return value)))))

;;; Converter implementations

(service-provider:define-service converter
  (:documentation
   "Providers convert payload to a particular wire-type."))
