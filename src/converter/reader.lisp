;;;; reader.lisp --- A converter that uses the Lisp reader/printer.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.converter)

(defun %normalize-type (type-specifier)
  (cond
    ((subtypep type-specifier 'string)
     'string)
    ((subtypep type-specifier 'vector)
     'vector)
    ((subtypep type-specifier 'integer)
     'integer)
    (t
     type-specifier)))

(defmethod wire->domain? ((converter   (eql :reader))
                          (wire-data   string)
                          (wire-schema t))
  (values converter wire-schema))

(defmethod domain->wire? ((converter     (eql :reader))
                          (domain-object t))
  (values converter
          'string
          (%normalize-type (type-of domain-object))))

(defmethod wire->domain ((converter   (eql :reader))
                         (wire-data   string)
                         (wire-schema t))
  (with-standard-io-syntax
    (read-from-string wire-data)))

(defmethod wire->domain :around ((converter   (eql :reader))
                                 (wire-data   string)
                                 (wire-schema t))
  (let ((expected-type wire-schema)
        (result        (call-next-method)))
    (unless (typep result expected-type)
      (error 'wire->domain-conversion-error
             :wire-schema      wire-schema
             :encoded          wire-data
             :domain-type      expected-type
             :format-control   "~@<The value is not ~A is not of the ~
                                expected type ~A.~@:>"
             :format-arguments `(,result ,expected-type)))
    result))

(defmethod domain->wire ((converter     (eql :reader))
                         (domain-object t))
  (values
   (with-standard-io-syntax
     (prin1-to-string domain-object))
   (%normalize-type (type-of domain-object))))
