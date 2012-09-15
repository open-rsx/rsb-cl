;;; mixins.lisp ---
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsb.serialization)


;;; `cache-binding-mixin' mixin class
;;

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
    (log1 :info "Binding thread local caches: ~{~A~^, ~}"
	  (mapcar #'first cache-bindings))
    (invoke-with-caches cache-bindings #'call-next-method)))


;;; `conversion-mixin' mixin class
;;

(defclass conversion-mixin ()
  ((converter :initarg  :converter
	      :accessor serialization-converter
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

(defmethod domain->wire ((serialization conversion-mixin)
			 (domain-object t))
  "Delegate conversion of DOMAIN-OBJECT to the converter stored in
SERIALIZATION."
  (domain->wire (serialization-converter serialization) domain-object))

(defmethod wire->domain ((serialization conversion-mixin)
			 (wire-data     t)
			 (wire-schema   t))
  "Delegate the conversion of WIRE-DATA, WIRE-SCHEMA to the converter
stored in SERIALIZATION."
  (wire->domain (serialization-converter serialization) wire-data wire-schema))
