;;;; connector-class.lisp --- Metaclass for connector classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

(defclass connector-class (standard-class)
  ((direction :type     direction
              :reader   connector-direction
              :documentation
              "Stores the direction of instances of the connector
class.")
   (wire-type :type     (or symbol list)
              :documentation
              "Stores the wire-type of instance of the connector
class.")
   (schemas   :initarg  :schemas
              :type     list
              :initform '()
              :documentation
              "Stores a list of schemas provided by the connector
class.")
   (options   :type     list
              :documentation
              "Stores options accepted by the connector class. Options
are mapped to initargs."))
  (:documentation
   "This metaclass can be used as the class of connector classes in
order to provide storage and retrieval (via methods on
`connector-direction', `connector-wire-type', `connector-schemas' and
`connector-options') for connector direction, wire-type, schemas and
options."))

(defmethod shared-initialize :before ((instance   connector-class)
                                     (slot-names t)
                                     &key
                                     wire-type
                                     direction
                                     (options nil options-supplied?))
  (when wire-type
    (setf (slot-value instance 'wire-type) (first wire-type)))
  (when direction
    (setf (slot-value instance 'direction) (first direction)))

  (when (or options-supplied? (not (slot-boundp instance 'options)))
    (setf (slot-value instance 'options) options)))

(defmethod closer-mop:compute-default-initargs ((class connector-class))
  ;; Added initargs for options with &slot specification.
  (reduce (lambda+ (default-initargs
                    (name &ign
                     &key (default t default-supplied?) &allow-other-keys))
            (if default-supplied?
                (list* (list name default
                             (compile nil `(lambda () ,default)))
                       (remove name default-initargs :key #'first))
                default-initargs))
          (connector-options class)
          :initial-value (call-next-method)))

(defmethod closer-mop:validate-superclass ((class      connector-class)
                                           (superclass standard-class))
  t)

(defmethod connector-wire-type ((connector connector-class))
  ;; Use wire-type stored in CONNECTOR or retrieve from superclasses
  ;; if necessary.
  (if (slot-boundp connector 'wire-type)
      (slot-value connector 'wire-type)
      (some #'connector-wire-type
            (closer-mop:class-direct-superclasses connector))))

(defmethod connector-schemas ((connector connector-class))
  ;; Retrieve supported schemas from CONNECTOR and its transitive
  ;; superclasses.
  (append (slot-value connector 'schemas)
          (mappend #'connector-schemas
                   (closer-mop:class-direct-superclasses connector))))

(defmethod connector-options ((connector connector-class))
  ;; Retrieve options from CONNECTOR and its transitive super-classes.
  ;; Option definitions in subclasses take precedence over definitions
  ;; in super-classes.
  (let+ (((&slots options) connector))
    (map-into options (curry #'%maybe-expand-option connector) options)
    (remove-duplicates
     (append options
             (mappend #'connector-options
                      (closer-mop:class-direct-superclasses connector)))
     :key      #'first
     :from-end t)))

;;; Utility functions

(defun+ %maybe-expand-option (class (&whole option name type &rest &ign))
  "Potentially expand the options description OPTION using information
from CLASS."
  (or (unless (eq type '&slot)
        option)
      (when-let ((slot (find name (closer-mop:class-direct-slots class)
                             :test #'member
                             :key  #'closer-mop:slot-definition-initargs)))
        `(,name
          ,(closer-mop:slot-definition-type slot)
          ,@(when-let ((initform (closer-mop:slot-definition-initform slot)))
              `(:default ,initform))
          ,@(when-let ((description (documentation slot t)))
              `(:description ,description))))
      (error "~@<~S specified for option ~S, but no slot with ~
              initarg ~:*~S in class ~S.~@:>"
             '&slot name class)))
