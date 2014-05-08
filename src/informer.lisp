;;;; informer.lisp --- Informers put events onto a bus.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass informer (participant
                    direction-mixin
                    rsb.ep:client
                    rsb.ep:broadcast-processor)
  ((direction                 :allocation :class
                              :initform :out)
   (type                      :initarg  :type
                              :type     (or symbol list)
                              :reader   informer-type
                              :initform t
                              :documentation
                              "Stores the type specifying the type of
event payloads supported by the informer.")
   (sequence-number-generator :type     function
                              :reader   informer-%sequence-number-generator
                              :initform (make-sequence-number-generator)
                              :documentation
                              "Stores a thread-safe sequence number
generation function which is used to generate sequence numbers for
events sent by this informer."))
  (:documentation
   "An informer is a participant that publishes events to a specific
channel. Other participants on the same channel or a channel that
includes the informer's channel can receive these events. It is
possible for multiple informers to send events for the same
channel."))

(defmethod send :before ((informer informer) (data event)
                         &key
                         unchecked?)
  (when unchecked?
    (return-from send))

  (let+ (((&accessors-r/o (scope participant-scope)
                          (type  informer-type)) informer))
    ;; Ensure that the type of DATA is a subtype of INFORMER's type.
    (unless (typep (event-data data) type)
      (error 'event-type-error
             :event         data
             :datum         data
             :expected-type type))

    ;; Ensure that the destination scope of DATA is identical to
    ;; INFORMER's scope.
    (unless (sub-scope?/no-coerce (event-scope data) scope)
      (error 'event-scope-error
             :event          data
             :expected-scope scope))))

(defmethod send ((informer informer) (event event)
                 &rest meta-data
                 &key
                 method
                 timestamps
                 causes)
  ;; Set EVENT's sequence number to our next sequence number and
  ;; origin to our id.
  (setf (event-sequence-number event)
        (funcall (informer-%sequence-number-generator informer))
        (event-origin event)
        (participant-id informer))

  ;; Set method if supplied.
  (when method
    (setf (event-method event) method))

  ;; Additional timestamps.
  (iter (for (key value) on timestamps :by #'cddr)
        (check-type value local-time:timestamp)
        (setf (timestamp event key) value))

  ;; Additional meta-data.
  (iter (for (key value) on meta-data :by #'cddr)
        (unless (member key '(:method :timestamps :causes :unchecked?)
                        :test #'eq)
          (check-type value (or string keyword real))
          (setf (meta-data event key) value)))

  ;; Additional causes.
  (when causes
    (appendf (event-causes event) causes))

  ;; Send EVENT.
  (rsb.ep:handle informer event)

  ;; Return EVENT to the client in case we created it on the fly.
  event)

(defmethod send ((informer informer) (data t)
                 &rest args
                 &key)
  (apply #'send informer
         (make-event (participant-scope informer) data)
         args))

(defmethod print-object ((object informer) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A ~S"
            (scope-string  (participant-scope object))
            (informer-type object))))

;;; `informer' creation

(defmethod make-participant-using-class ((class     class)
                                         (prototype informer)
                                         (scope     scope)
                                         &key)
  ;; Connect the processor of CONFIGURATOR to INFORMER as an event
  ;; handler and return the ready-to-use `informer' instance.
  (let* ((informer     (call-next-method))
         (configurator (rsb.ep:client-configurator informer)))
    (push (rsb.ep:configurator-processor configurator)
          (rsb.ep:handlers informer))
    informer))

(defmethod make-informer ((scope scope)
                          (type  t)
                          &rest args &key
                          (transports (transport-options))
                          (converters (default-converters))
                          transform
                          error-policy)
  (declare (ignore transform error-policy))
  ;; Translate different kinds of errors into
  ;; `informer-creation-error' errors.
  (with-condition-translation
      (((error informer-creation-error)
        :kind       :informer
        :scope      scope
        :transports transports
        :type       type))
    (apply #'make-participant 'informer scope
           :transports transports
           :converters converters
           :type       type
           args)))

(define-participant-creation-uri-methods informer
  (scope puri:uri) (type t))

(define-participant-creation-restart-method informer
  (scope scope) (type t))
(define-participant-creation-restart-method informer
  (scope puri:uri) (type t))
