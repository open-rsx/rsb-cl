;;;; mixins.lisp --- Mixins classes used/provided by the rsb module.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
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

(defmethod print-items:print-items append ((object uuid-mixin))
  (let+ (((&slots-r/o id) object))
    `((:id ,id " ~/rsb::print-id/"))))

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

;;; `direction-mixin'

(defclass direction-mixin ()
  ((direction :type     direction
              :allocation :class
              :reader   participant-direction))
  (:documentation
   "This mixin class is intended to be mixed into classes to which a
    communication direction can be associated.

    Examples are

      `reader'   -> :in
      `listener' -> :in
      `informer' -> :out"))

;;; meta-data and timestamp plist mixins

(define-plist-data-mixin meta-data)

(define-plist-data-mixin timestamp)

;;; `converters-mixin'

(defclass converters-mixin ()
  ((converters :initarg  :converters
               :type     list
               :initform '()
               :reader   participant-converters
               :documentation
               "Stores a list of the converters available for use in
                connectors of the participant. Each element is of the
                form

                  (WIRE-TYPE . CONVERTER)

                ."))
  (:documentation
   "This mixin class adds a list of converters associated to
    wire-types."))

(defmethod participant-converter ((participant converters-mixin)
                                  (wire-type   t)
                                  &key &allow-other-keys)
  ;; Return the converter for WIRE-TYPE that is used by the connectors
  ;; of PARTICIPANT.
  (mapcar #'cdr
          (remove wire-type (participant-converters participant)
                  :key      #'car
                  :test-not #'subtypep)))

;;; `error-hook-mixin'

(defclass error-hook-mixin ()
  ((error-hook :type     list #|of function|#
               :initform '()
               :documentation
               "Stores a list of functions to call in case of
                errors."))
  (:documentation
   "This mixin class adds an error hook which is automatically
    connected to a specified error policy when an instance is
    created."))

(defmethod participant-error-hook ((participant error-hook-mixin))
  (hooks:object-hook participant 'error-hook))

(defmethod make-participant-using-class ((class     class)
                                         (prototype error-hook-mixin)
                                         (scope     scope)
                                         &rest args &key
                                         (error-policy nil error-policy-supplied?))
  (let ((participant (if error-policy-supplied?
                         (apply #'call-next-method class prototype scope
                                (remove-from-plist args :error-policy))
                         (call-next-method))))
    (when error-policy
      (hooks:add-to-hook (participant-error-hook participant) error-policy))
    participant))
