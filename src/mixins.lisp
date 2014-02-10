;;;; mixins.lisp --- Mixins classes used/provided by the rsb module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; `uuid-mixin'

(defclass uuid-mixin ()
  ((id :type     uuid:uuid
       :documentation
       "Stores the unique id of the object."))
  (:documentation
   "This class can be mixed into classes that need a uuid in there
    instances."))

(defmethod shared-initialize :after ((instance  uuid-mixin)
                                     (slot-name t)
                                     &key
                                     id)
  (cond
   ;; If ID has been supplied, set the slot value to it.
   (id
    (setf (slot-value instance 'id)
          (if (stringp id) (uuid:make-uuid-from-string id) id)))
   ;; If ID has not been supplied and the slot is unbound, set
   ;; it. Use the value of `*id-random-state*' to give clients
   ;; control over the generated ids.
   ((not (slot-boundp instance 'id))
    (setf (slot-value instance 'id)
          (let ((uuid::*uuid-random-state* *id-random-state*))
            (uuid:make-v4-uuid))))))

;;; `scope-mixin'

(defclass scope-mixin ()
  ((scope :type     scope
          :documentation
          "Stores the scope that is associated to the instance. For
           long-lived and reused objects, interned scopes should be
           stored."))
  (:default-initargs
   :scope (missing-required-initarg 'scope-mixin :scope))
  (:documentation
   "This mixin class is intended to be mixed into classes instances of
    which have a mandatory associated scope."))

(defmethod shared-initialize :after ((instance   scope-mixin)
                                     (slot-names t)
                                     &key
                                     scope
                                     (intern-scope? t))
  (when scope
    (setf (slot-value instance 'scope)
          (make-scope scope :intern? intern-scope?))))

;;; `uri-mixin'

(defclass uri-mixin ()
  ((uri :type     puri:uri
        :documentation
        "Stores the URI of the object."))
  (:documentation
   "This mixin class is intended to be mixed into classes instance of
    which have an associated URI."))

(defmethod shared-initialize :after ((instance   uri-mixin)
                                     (slot-names t)
                                     &key
                                     uri)
  (when uri
    (setf (slot-value instance 'uri) (puri:uri uri))))

;;; meta-data and timestamp plist mixins

(define-plist-data-mixin meta-data)

(define-plist-data-mixin timestamp)
