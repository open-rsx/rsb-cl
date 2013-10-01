;;;; connector-class.lisp --- Metaclass for connector classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport)

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
              :initform nil
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
  (let ((default-initargs (call-next-method)))
    ;; Added initargs for options with &slot specification.
    (iter (for option in (connector-options class))
          (let+ (((name &ign
                   &key (default t default-supplied?) &allow-other-keys)
                  option))
            (when default-supplied?
              (setf default-initargs
                    (cons (list name default
                                (compile nil `(lambda () ,default)))
                          (remove name default-initargs :key #'first))))))
    default-initargs))

(defmethod connector-wire-type ((class connector-class))
  "Use wire-type stored in CLASS or retrieve from super-classes if
necessary. "
  (if (slot-boundp class 'wire-type)
      (slot-value class 'wire-type)
      (some #'connector-wire-type
            (closer-mop:class-direct-superclasses class))))

(defmethod connector-schemas ((class connector-class))
  "Retrieve supported schemas from CLASS and its transitive
super-classes."
  (append (slot-value class 'schemas)
          (mappend #'connector-schemas
                   (closer-mop:class-direct-superclasses class))))

(defmethod connector-options ((class connector-class))
  "Retrieve options from CLASS and its transitive super-classes.
Option definitions in subclasses take precedence over definitions in
super-classes."
  (let+ (((&slots options) class))
    (map-into options (curry #'%maybe-expand-option class) options)
    (remove-duplicates
     (append (slot-value class 'options)
             (mappend #'connector-options
                      (closer-mop:class-direct-superclasses class)))
     :key      #'first
     :from-end t)))

(defmethod closer-mop:validate-superclass ((class      connector-class)
                                           (superclass standard-class))
  t)

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
