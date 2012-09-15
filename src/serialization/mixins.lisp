;;;; mixins.lisp ---
;;;;
;;;; Copyright (C) 2012, 2014 Jan Moringen
;;;;

(cl:in-package #:rsb.serialization)

;;; `cache-binding-mixin' mixin class

(defclass cache-binding-mixin ()
  ()
  (:documentation
   "This mixin "))

(defgeneric collect-caches (serialization)
  (:method-combination append)
  (:documentation
   "TODO(jmoringe): document"))

(defmethod collect-caches :around ((serialization cache-binding-mixin))
  (remove-duplicates (call-next-method) :key #'first :test #'eq))

(defmethod execute :around ((serialization cache-binding-mixin))
  (let ((cache-bindings (collect-caches serialization)))
    (log:info "Binding thread-local caches: ~{~A~^, ~}"
              (mapcar #'first cache-bindings))
    (call-with-caches cache-bindings #'call-next-method)))

;;; `conversion-mixin' mixin class

(defclass conversion-mixin ()
  ((converter :initarg  :converter
              :accessor serialization-converter
              :documentation
              "A converter to which the actual conversion work is
               delegated."))
  (:default-initargs
   :converter (missing-required-initarg 'conversion-mixin :converter))
  (:documentation
   "This mixin adds methods on `domain->wire' and `wire->domain' for
    the subclass which delegate the conversion tasks to a stored
    converter."))

(defmethod domain->wire ((serialization conversion-mixin)
                         (domain-object t))
  ;; Delegate conversion of DOMAIN-OBJECT to the converter stored in
  ;; SERIALIZATION.
  (domain->wire (serialization-converter serialization) domain-object))

(defmethod wire->domain ((serialization conversion-mixin)
                         (wire-data     t)
                         (wire-schema   t))
  ;; Delegate the conversion of WIRE-DATA, WIRE-SCHEMA to the
  ;; converter stored in SERIALIZATION.
  (wire->domain (serialization-converter serialization) wire-data wire-schema))
