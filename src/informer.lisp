;;;; informer.lisp --- Informers put events onto a bus.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
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
                               event payloads supported by the
                               informer.")
   (sequence-number-generator :type     function
                              :reader   informer-%sequence-number-generator
                              :initform (make-sequence-number-generator)
                              :documentation
                              "Stores a thread-safe sequence number
                               generation function which is used to
                               generate sequence numbers for events
                               sent by this informer."))
  (:documentation
   "A participant that publishes events to a specific channel.

    Other participants on the same channel or a channel that includes
    the informer's channel can receive these events. It is possible
    for multiple informers to send events for the same channel."))

(register-participant-class 'informer)

(defmethod print-items:print-items append ((object informer))
  `((:type ,(informer-type object) " ~A")))

(declaim (inline %check-send-event))

(defun %check-send-event (informer-scope informer-type event)
  (let+ (((&structure-r/o event- data scope) event))
    ;; Ensure that the type of DATA is a subtype of INFORMER-TYPE.
    (unless (typep data informer-type)
      (error 'event-type-error
             :event         event
             :datum         data
             :expected-type informer-type))

    ;; Ensure that the destination scope of DATA is identical to
    ;; INFORMER-SCOPE.
    (unless (sub-scope?/no-coerce scope informer-scope)
      (error 'event-scope-error
             :event          event
             :expected-scope scope))))

(defmethod send ((informer informer) (data event)
                 &rest meta-data  ; TODO make this a keyword parameter
                 &key
                 method
                 timestamps
                 causes
                 unchecked?
                 no-fill?)
  (let ((meta-data (remove-from-plist
                    meta-data :method :timestamps :causes
                              :unchecked? :no-fill?)))
    ;; Check the event w.r.t. to restrictions imposed by INFORMER.
    (unless unchecked?
      (%check-event-arguments timestamps meta-data causes)
      (let ((informer-scope (participant-scope informer))
            (informer-type  (informer-type     informer)))
        (%check-send-event informer-scope informer-type data)))

    ;; Set DATA's sequence number to our next sequence number and
    ;; origin to our id.
    (unless no-fill?
      (setf (event-sequence-number data)
            (funcall (informer-%sequence-number-generator informer))
            (event-origin data)
            (participant-id informer)))

    ;; Set method, additional timestamps, meta-data and causes, if
    ;; supplied.
    (when method
      (setf (event-method data) method))
    (when timestamps
      (appendf (slot-value data 'timestamp) timestamps))
    (when meta-data
      (appendf (slot-value data 'meta-data) meta-data))
    (when causes
      (appendf (event-causes data) causes))

    ;; Send the event and return it to the caller.
    (rsb.ep:handle informer data)
    data))

(defmethod send ((informer informer) (data t) &rest args &key)
  (let ((event (make-instance 'event
                              :scope (participant-scope informer)
                              :data  data)))
    (apply #'send informer event args)))

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
