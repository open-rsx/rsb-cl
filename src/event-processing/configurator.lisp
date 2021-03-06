;;;; configurator.lisp --- Superclass for configurator classes.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

(defclass configurator (scope-mixin
                        error-policy-mixin)
  ((scope      :reader   configurator-scope)
   (direction  :initarg  :direction
               :type     direction
               :reader   configurator-direction
               :documentation
               "Stores the direction of the communication situation
                the configurator is responsible for.")
   (connectors :initarg  :connectors
               :type     list
               :accessor configurator-connectors
               :initform '()
               :documentation
               "Stores the list of connector instances the client uses
                to access the bus.")
   (processor  :initarg  :processor
               :reader   configurator-processor
               :documentation
               "Stores the processor instance that handles incoming or
                outgoing events.")
   (transform  :initarg  :transform
               :reader   configurator-transform
               :initform nil
               :documentation
               "Stores the transform which should be applied to
                processed events."))
  (:default-initargs
   :direction (missing-required-initarg 'configurator :direction))
  (:documentation
   "Intended as a superclass of configurator classes for specific
    directions.

    Every configurator instance has a participant instance as its
    \"client\"."))

(defmethod shared-initialize :after ((instance   configurator)
                                     (slot-names t)
                                     &key)
  ;; Create a processor if none has been supplied.
  (unless (slot-boundp instance 'processor)
    (setf (slot-value instance 'processor)
          (apply #'make-processor instance
                 (when-let ((transform (configurator-transform instance)))
                   (list :transform transform)))))

  ;; Propagate the selected error-policy into the processor and
  ;; potentially connectors.
  (setf (processor-error-policy instance)
        (processor-error-policy instance)))

(defmethod (setf processor-error-policy) :around ((new-value    t)
                                                  (configurator configurator))
  (let+ (((&structure-r/o configurator- processor connectors) configurator))
    (log:trace "~@<~A is installing new error policy ~A in processor ~A~@:>"
               configurator new-value processor)
    (setf (processor-error-policy processor) new-value)

    (iter (for connector in connectors)
          (log:trace "~@<~A is installing new error policy ~A in connector ~A~@:>"
                     configurator new-value connector)
          (setf (processor-error-policy connector) new-value))

    (call-next-method)))

(defmethod (setf configurator-connectors) :around ((new-value    list)
                                                   (configurator configurator))
  (let ((old-value (configurator-connectors configurator)))
    (prog1
        (call-next-method)
      (let ((added   (set-difference new-value old-value))
            (removed (set-difference old-value new-value)))
        (log:trace "~@<~A added connectors ~{~A~^, ~}~@:>" configurator added)
        (log:trace "~@<~A removed connectors ~{~A~^, ~}~@:>" configurator removed)

        (iter (for connector in added)
              (notify configurator connector :connector-added))
        (iter (for connector in removed)
              (notify configurator connector :connector-removed))))))

(defmethod collect-processor-mixins append ((configurator configurator))
  (append '(error-policy-mixin)
          (when (configurator-transform configurator) '(transform-mixin))))


(#+sbcl sb-ext:defglobal #-sbcl defvar **processor-classes**
  (make-hash-table :test #'equal))

(#+sbcl sb-ext:defglobal #-sbcl defvar **processor-classes-lock**
        (bt:make-lock "Processor classes"))

#+sbcl (declaim (sb-ext:always-bound **processor-classes**
                                     **processor-classes-lock**))

(defun ensure-processor-class (mixins)
  (bt:with-lock-held (**processor-classes-lock**)
    (ensure-gethash
     mixins **processor-classes**
     (make-instance 'standard-class
                    :name                (with-standard-io-syntax
                                           (let ((*print-pretty* nil))
                                             (prin1-to-string mixins)))
                    :direct-superclasses (mapcar #'find-class mixins)))))

(defmethod make-processor ((configurator configurator)
                           &rest args
                           &key &allow-other-keys)
  (let ((class (ensure-processor-class
                (collect-processor-mixins configurator))))
    (apply #'make-instance class args)))

(defmethod detach ((configurator configurator))
  ;; Detach all connectors from the scope of CONFIGURATOR.
  (log:trace "~@<~A is detaching ~D connector~:P~@:>"
             configurator (length (configurator-connectors configurator)))
  (iter (for connector in (configurator-connectors configurator))
        ;; Give each connector ten seconds to detach. If one takes
        ;; longer, skip it.
        (with-restart-and-timeout (10)
          (notify configurator connector :connector-removed))))

;;; Connectors

(defmethod notify ((configurator configurator)
                   (connector    t)
                   (action       (eql :connector-added)))
  (let+ (((&accessors-r/o
           (scope        configurator-scope)
           (error-policy processor-error-policy))
          configurator))
    (log:trace "~@<~A is installing new error policy ~A in connector ~A~@:>"
               configurator error-policy connector)
    (setf (processor-error-policy connector) error-policy)

    (log:trace "~@<~A is attaching connector ~A to scope ~A~@:>"
               configurator connector scope)
    (notify connector scope :attached)))

(defmethod notify ((configurator configurator)
                   (connector    t)
                   (action       (eql :connector-removed)))
  (let+ (((&structure-r/o configurator- scope) configurator))
    (log:trace "~@<~A is detaching connector ~A from ~A~@:>"
               configurator connector scope)
    (notify connector scope :detached)))
