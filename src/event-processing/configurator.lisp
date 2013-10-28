;;;; configurator.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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
               "The direction of the communication situation the
configurator is responsible for.")
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
   :error-policy #'log-error)
  (:documentation
   "This class is intended to be used as a superclass of configurator
classes for specific directions. Every configurator instance has a
participant instance as its \"client\"."))

(defmethod shared-initialize :after ((instance   configurator)
                                     (slot-names t)
                                     &key
                                     processor)
  ;; Create a processor if none has been supplied.
  (unless processor
    (setf (slot-value instance 'processor)
          (apply #'make-processor instance
                 (when-let ((transform (configurator-transform instance)))
                   (list :transform transform)))))

  ;; Propagate the selected error policy into the processor and
  ;; potentially connectors.
  (setf (processor-error-policy instance)
        (processor-error-policy instance)))

(defmethod (setf processor-error-policy) :around  ((new-value    t)
                                                   (configurator configurator))
  (let+ (((&accessors-r/o (processor  configurator-processor)
                          (connectors configurator-connectors)) configurator))
    (log1 :trace configurator "Installing new error policy ~S in processor ~S"
          new-value processor)
    (setf (processor-error-policy processor) new-value)

    (iter (for connector in connectors)
          (log1 :trace configurator "Installing new error policy ~S in connector ~S"
                new-value connector)
          (setf (processor-error-policy connector) new-value))

    (call-next-method)))

(defmethod (setf configurator-connectors) :around ((new-value    list)
                                                   (configurator configurator))
  (let ((old-value (configurator-connectors configurator)))
    (prog1
        (call-next-method)
      (let ((added   (set-difference new-value old-value))
            (removed (set-difference old-value new-value)))
        (log1 :trace configurator "Added   connectors ~{~S~^, ~}" added)
        (log1 :trace configurator "Removed connectors ~{~S~^, ~}" removed)

        (iter (for connector in added)
              (notify configurator connector :connector-added))
        (iter (for connector in removed)
              (notify configurator connector :connector-removed))))))

(defmethod collect-processor-mixins append ((configurator configurator))
  (let+ (((&accessors-r/o (transform configurator-transform)) configurator))
    (append '(error-policy-mixin) (when transform '(transform-mixin)))))

(defmethod make-processor ((configurator configurator)
                           &rest args
                           &key &allow-other-keys)
  (apply #'make-instance
         (ensure-processor-class (collect-processor-mixins configurator))
         args))

(defmethod detach ((configurator configurator))
  "Detach all connectors from the scope of CONFIGURATOR."
  (log1 :trace configurator "Detaching ~D connector~:P"
        (length (configurator-connectors configurator)))
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
           (error-policy processor-error-policy)) configurator))
    (log1 :trace configurator "Installing new error policy ~S in connector ~S" error-policy connector)
    (setf (processor-error-policy connector) error-policy)

    (log1 :trace configurator "Attaching connector ~S to scope ~S" connector scope)
    (notify connector scope :attached)))

(defmethod notify ((configurator configurator)
                   (connector    t)
                   (action       (eql :connector-removed)))
  (let+ (((&accessors-r/o (scope configurator-scope)) configurator))
    (log1 :trace configurator "Detaching connector ~S from ~S" connector scope)
    (notify connector scope :detached)))
