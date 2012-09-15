;;;; protocol-buffer.lisp ---
;;;;
;;;; Copyright (C) 2012, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.serialization)

(defclass protocol-buffer (conversion-mixin)
  ((expose-wire-schema?  :initarg  :expose-wire-schema?
                         :type     boolean
                         :accessor serialization-expose-wire-schema?
                         :documentation
                         "")
   (expose-payload-size? :initarg  :expose-payload-size?
                         :type     boolean
                         :accessor serialization-expose-payload-size?
                         :documentation
                         ""))
  (:documentation
   "TODO(jmoringe): document"))

(service-provider:register-provider/class
 'serialization :rsb-0.8/procotol-buffer :class 'protocol-buffer)

(defmethod collect-caches append ((serialization protocol-buffer))
  (list (list '*bytes->keyword-cache*     #'make-bytes->keyword-cache)
        (list '*keyword->bytes-cache*     #'make-keyword->bytes-cache)
        (list '*bytes->wire-schema-cache* #'make-bytes->wire-schema-cache)
        (list '*wire-schema->bytes-cache* #'make-wire-schema->bytes-cache)))

(defmethod notification->event ((serialization protocol-buffer)
                                (notification  rsb.protocol:notification))
  ;; Convert NOTIFICATION to an `event' instance using CONVERTER for
  ;; the payload. Return the decoded event. The optional parameter
  ;; DATA can be used to supply encoded data that should be used
  ;; instead of the data contained in NOTIFICATION.
  (let+ (((&structure-r/o serialization- expose-wire-schema? expose-payload-size?)
          serialization)
         ((&flet event-id->cons (event-id)
            (cons (uuid:byte-array-to-uuid
                   (rsb.protocol:event-id-sender-id event-id))
                  (rsb.protocol:event-id-sequence-number event-id))))
         ((&accessors-r/o
           (scope       rsb.protocol:notification-scope)
           (event-id    rsb.protocol:notification-event-id)
           (method      rsb.protocol:notification-method)
           (wire-schema rsb.protocol:notification-wire-schema)
           (payload     rsb.protocol:notification-data)
           (meta-data   rsb.protocol:notification-meta-data)
           (causes      rsb.protocol:notification-causes)) notification)
         ((&accessors-r/o
           (sender-id       rsb.protocol:event-id-sender-id)
           (sequence-number rsb.protocol:event-id-sequence-number)) event-id)
         (wire-schema (bytes->wire-schema wire-schema))
         (data        (wire->domain serialization payload wire-schema))
         (event       (make-instance
                       'rsb:event
                       :origin            (uuid:byte-array-to-uuid sender-id)
                       :sequence-number   sequence-number
                       :scope             (make-scope (bytes->string scope))
                       :method            (unless (emptyp method)
                                            (bytes->keyword method))
                       :causes            (map 'list #'event-id->cons causes)
                       :data              data
                       :create-timestamp? nil)))

    ;; "User infos" and timestamps
    (when meta-data
      ;; "User infos"
      (iter (for user-info in-vector (rsb.protocol:event-meta-data-user-infos meta-data))
            (setf (meta-data event (bytes->keyword
                                    (rsb.protocol:user-info-key user-info)))
                  (bytes->string (rsb.protocol:user-info-value user-info))))

      ;; Set :create timestamp, if present
      (unless (zerop (rsb.protocol:event-meta-data-create-time meta-data))
        (setf (timestamp event :create)
              (unix/microseconds->timestamp
               (rsb.protocol:event-meta-data-create-time meta-data))))
      ;; Set :send timestamp, if present
      (unless (zerop (rsb.protocol:event-meta-data-send-time meta-data))
        (setf (timestamp event :send)
              (unix/microseconds->timestamp
               (rsb.protocol:event-meta-data-send-time meta-data))))
      ;; Set :receive timestamp
      (setf (timestamp event :receive) (local-time:now))

      (iter (for user-time in-vector (rsb.protocol:event-meta-data-user-times meta-data))
            (setf (timestamp event (bytes->keyword
                                    (rsb.protocol:user-time-key user-time)))
                  (unix/microseconds->timestamp
                   (rsb.protocol:user-time-timestamp user-time)))))

    ;; When requested, store transport metrics as meta-data items.
    (when expose-wire-schema?
      (setf (rsb:meta-data event :rsb.transport.wire-schema)
            wire-schema))
    (when expose-payload-size?
      (setf (rsb:meta-data event :rsb.transport.payload-size)
            (length payload)))

    event))

(defmethod event->notification ((serialization protocol-buffer)
                                (event         event))
  ;; Convert EVENT into a `rsb.protocol:notification' instance.
  ;; Set the :send timestamp of EVENT to enable the caller to read it.
  (setf (timestamp event :send) (local-time:now))

  ;; Put EVENT into a notification.
  (let+ (((&structure-r/o event- origin sequence-number scope
                          method data meta-data timestamps causes) event)
         ((&values wire-data wire-schema)
          (domain->wire serialization data))
         (event-id     (make-instance
                        'rsb.protocol:event-id
                        :sender-id       (uuid:uuid-to-byte-array origin)
                        :sequence-number sequence-number))
         (meta-data1   (make-instance
                        'rsb.protocol:event-meta-data
                        :create-time (timestamp->unix/microseconds
                                      (getf timestamps :create))
                        :send-time   (timestamp->unix/microseconds
                                      (getf timestamps :send))))
         (notification (make-instance
                        'rsb.protocol:notification
                        :event-id        event-id
                        :scope           (string->bytes (scope-string scope))
                        :wire-schema     (wire-schema->bytes wire-schema)
                        :data            wire-data
                        :meta-data       meta-data1)))
    ;; Store the method of the event in the new notification if the
    ;; event has one.
    (when method
      (setf (rsb.protocol:notification-method notification)
            (keyword->bytes method)))

    ;; Add META-DATA.
    (iter (for (key value) on meta-data :by #'cddr)
          (vector-push-extend
           (make-instance 'user-info
                          :key   (keyword->bytes key)
                          :value (string->bytes value))
           (rsb.protocol:event-meta-data-user-infos meta-data1)))

    ;; Add TIMESTAMPS.
    (iter (for (key value) on timestamps :by #'cddr)
          (unless (eq key :create) ;; the event should not have :send,
            ;; :receive or :deliver at this
            ;; point
            (vector-push-extend
             (make-instance 'user-time
                            :key       (keyword->bytes key)
                            :timestamp (timestamp->unix/microseconds value))
             (rsb.protocol:event-meta-data-user-times meta-data1))))

    ;; Add CAUSES.
    (iter (for (origin-id . sequence-number) in causes)
          (vector-push-extend
           (make-instance 'rsb.protocol:event-id
                          :sender-id       (uuid:uuid-to-byte-array
                                            origin-id)
                          :sequence-number sequence-number)
           (rsb.protocol:notification-causes notification)))

    ;; Return the complete notification instance.
    notification))
