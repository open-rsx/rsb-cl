;;;; connector-class.lisp --- Metaclass for connector classes.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

(defclass connector-class (standard-class)
  ((transport :accessor connector-%transport
              :documentation
              "Stores the transport instance to which the transport
               class and its instances belong.")
   (direction :type     direction
              :documentation
              "Stores the direction of instances of the connector
               class.")
   (options   :type     list
              :documentation
              "Stores options accepted by the connector class. Options
               are mapped to initargs."))
  (:documentation
   "This metaclass can be used as the class of connector classes in
    order to provide storage and retrieval (via methods on
    `connector-direction', `connector-wire-type', `connector-schemas'
    and `connector-options') for connector direction, wire-type,
    schemas and options."))

(defmethod shared-initialize :before ((instance   connector-class)
                                      (slot-names t)
                                      &key
                                      transport
                                      direction
                                      (options nil options-supplied?))
  (when transport
    (setf (connector-%transport instance)
          (let ((transport (first transport)))
            (typecase transport
              ((or symbol cons)
               (service-provider:find-provider 'transport transport))
              (t
               transport)))))

  (when direction
    (setf (slot-value instance 'direction) (first direction)))

  (when (or options-supplied? (not (slot-boundp instance 'options)))
    (setf (slot-value instance 'options) options)))

(defmethod closer-mop:class-direct-default-initargs ((class connector-class))
  ;; Add initargs for options with &slot specification.
  (reduce (lambda+ (default-initargs
                    (name &ign
                     &key (default t default-supplied?) &allow-other-keys))
            (if default-supplied?
                (list* (list name default
                             (compile nil `(lambda () ,default)))
                       (remove name default-initargs :key #'first))
                default-initargs))
          (connector-direct-options class)
          :initial-value (call-next-method)))

(defmethod closer-mop:validate-superclass ((class      connector-class)
                                           (superclass standard-class))
  t)

(defmethod connector-transport ((connector connector-class))
  (if (slot-boundp connector 'transport)
      (slot-value connector 'transport)
      (some #'connector-transport
            (closer-mop:class-direct-superclasses connector))))

(defmethod connector-direction ((connector connector-class))
  (if (slot-boundp connector 'direction)
      (slot-value connector 'direction)
      (some #'connector-direction
            (closer-mop:class-direct-superclasses connector))))

(defmethod connector-direct-options ((connector connector-class))
  (mapcar (curry #'%maybe-expand-option connector)
          (slot-value connector 'options)))

(defmethod connector-options ((connector connector-class))
  ;; Retrieve options from CONNECTOR and its transitive super-classes.
  ;; Option definitions in subclasses take precedence over definitions
  ;; in super-classes.
  (remove-duplicates
   (append (connector-direct-options connector)
           (mappend #'connector-options
                    (closer-mop:class-direct-superclasses connector)))
   :key      #'first
   :from-end t))

;;; Delegations

(macrolet ((define-connector-class-accessor (name &optional to-transport?)
             (let ((argument (if to-transport? 'transport 'connector)))
               `(progn
                  ,@(when to-transport?
                      `((defmethod ,name ((,argument connector-class))
                          (,name (connector-transport ,argument)))))

                  (defmethod ,name ((,argument standard-object))
                    ;; Default behavior is to retrieve the value from
                    ;; the class of ARGUMENT.
                    (,name (class-of ,argument)))

                  (defmethod ,name ((,argument class))
                    ;; Stop if we hit a class which is not a
                    ;; `connector-class'.
                    (values))))))

  (define-connector-class-accessor transport-schemas   t)
  (define-connector-class-accessor transport-wire-type t)
  (define-connector-class-accessor transport-remote?   t)

  (define-connector-class-accessor connector-transport)
  (define-connector-class-accessor connector-direction)
  (define-connector-class-accessor connector-options))

;;; Utility functions

(defun+ %maybe-expand-option (class (&whole option name type &rest args))
  ;; Potentially expand the options description OPTION using
  ;; information from CLASS.
  (let+ (((&flet slot->option (slot)
            `(,name
              ,(closer-mop:slot-definition-type slot)
              ,@(when-let ((initform (closer-mop:slot-definition-initform slot)))
                  `(:default ,initform))
              ,@(when-let ((description (documentation slot t)))
                  `(:description ,description))))))
    (cond
      ((not (eq type '&slot))
       option)
      ((not args)
       (if-let ((slot (find name (closer-mop:class-direct-slots class)
                            :test #'member
                            :key  #'closer-mop:slot-definition-initargs)))
         (slot->option slot)
         (error "~@<~S specified for option ~S, but no slot with ~
                 initarg ~:*~S in class ~S.~@:>"
                '&slot name class)))
      (t
       (let ((slot-name (first args)))
         (if-let ((slot (find slot-name (closer-mop:class-direct-slots class)
                              :key #'closer-mop:slot-definition-name)))
           (slot->option slot)
           (error "~@<~S with slot name ~S specified for option ~S, ~
                   but no slot named ~2:*~S~* in class ~S.~@:>"
                  '&slot slot-name name class)))))))
