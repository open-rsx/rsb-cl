;;;; conversion.lisp --- Event <-> notification conversion for Spread transport.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport.spread)

;;; Notification -> Event

(defun notification->event (pool converter notification
                            &key
                            expose-wire-schema?
                            expose-payload-size?)
  "Try to convert NOTIFICATION into an event. This may not be possible
in a single step since NOTIFICATION can be a part of an event that has
been fragmented into multiple notifications."
  (if (<= (fragmented-notification-num-data-parts notification) 1)

      ;; When the event has been transmitted as a single notification,
      ;; an assembly step is not required.
      (one-notification->event
       converter
       (fragmented-notification-notification notification)
       :expose-wire-schema?  expose-wire-schema?
       :expose-payload-size? expose-payload-size?)

      ;; When the event has been fragmented into multiple
      ;; notifications, try to assemble for each
      ;; notification. `merge-fragment' returns nil until all
      ;; fragments have arrived.
      (when-let ((assembly (merge-fragment pool notification)))
        (one-notification->event
         converter
         (fragmented-notification-notification
          (aref (assembly-fragments assembly) 0))
         :data                (assembly-concatenated-data assembly)
         :expose-wire-schema?  expose-wire-schema?
         :expose-payload-size? expose-payload-size?))))

(defun one-notification->event (converter notification
                                &key
                                data
                                expose-wire-schema?
                                expose-payload-size?)
  "Convert NOTIFICATION to an `event' instance using CONVERTER for the
payload. Return the decoded event. The optional parameter DATA can be
used to supply encoded data that should be used instead of the data
contained in NOTIFICATION."
  (let+ (((&flet event-id->cons (event-id)
            (cons (uuid:byte-array-to-uuid (event-id-sender-id event-id))
                  (event-id-sequence-number event-id))))
         ((&accessors-r/o
           (scope       notification-scope)
           (event-id    notification-event-id)
           (method      notification-method)
           (wire-schema notification-wire-schema)
           (payload     notification-data)
           (meta-data   notification-meta-data)
           (causes      notification-causes)) notification)
         ((&accessors-r/o
           (sender-id       event-id-sender-id)
           (sequence-number event-id-sequence-number)) event-id)
         (wire-schema (bytes->wire-schema wire-schema))
         (data*       (rsb.converter:wire->domain
                       converter (or data payload)
                       wire-schema))
         (event       (make-instance
                       'rsb:event
                       :origin            (uuid:byte-array-to-uuid sender-id)
                       :sequence-number   sequence-number
                       :scope             (make-scope (bytes->string scope))
                       :method            (unless (emptyp method)
                                            (bytes->keyword method))
                       :causes            (map 'list #'event-id->cons causes)
                       :data              data*
                       :create-timestamp? nil)))

    ;; "User infos" and timestamps
    (when meta-data
      ;; "User infos"
      (iter (for user-info in-vector (event-meta-data-user-infos meta-data))
            (setf (rsb:meta-data event (bytes->keyword
                                        (user-info-key user-info)))
                  (bytes->string (user-info-value user-info))))

      ;; Set :create timestamp, if present
      (unless (zerop (event-meta-data-create-time meta-data))
        (setf (timestamp event :create)
              (unix-microseconds->timestamp
               (event-meta-data-create-time meta-data))))
      ;; Set :send timestamp, if present
      (unless (zerop (event-meta-data-send-time meta-data))
        (setf (timestamp event :send)
              (unix-microseconds->timestamp
               (event-meta-data-send-time meta-data))))
      ;; Set :receive timestamp
      (setf (timestamp event :receive) (local-time:now))

      (iter (for user-time in-vector (event-meta-data-user-times meta-data))
            (setf (timestamp event (bytes->keyword
                                    (user-time-key user-time)))
                  (unix-microseconds->timestamp
                   (user-time-timestamp user-time)))))

    ;; When requested, store transport metrics as meta-data items.
    (when expose-wire-schema?
      (setf (rsb:meta-data event :rsb.transport.wire-schema)
            wire-schema))
    (when expose-payload-size?
      (setf (rsb:meta-data event :rsb.transport.payload-size)
            (length (or data payload))))

    event))

;;; Event -> Notification

(declaim (ftype (function (t event positive-fixnum) list) event->notifications))

(defun event->notifications (converter event max-fragment-size)
  "Convert EVENT into one or more notifications. More than one
notification is required when data contained in event does not fit
into one notification."
  ;; Set the :send timestamp of EVENT to enable the caller to read it.
  (setf (timestamp event :send) (local-time:now))

  ;; Put EVENT into one or more notifications.
  (let+ (((&accessors-r/o (origin          event-origin)
                          (sequence-number event-sequence-number)
                          (scope           event-scope)
                          (method          event-method)
                          (data            event-data)
                          (meta-data       rsb:event-meta-data)
                          (timestamps      event-timestamps)
                          (causes          event-causes)) event)
         ((&values wire-data wire-schema)
          (rsb.converter:domain->wire converter data))
         (data-size (length wire-data)))
    (declare (type octet-vector wire-data))

    (iter (with remaining     =     data-size)
          (with offset        =     0)
          (for  i             :from 0)
          (let* ((notification  (apply #'make-notification
                                      sequence-number origin
                                      (when (first-iteration-p)
                                        (list scope method wire-schema
                                              meta-data timestamps causes))))
                 (fragment      (make-instance 'fragmented-notification
                                               :notification   notification
                                               :num-data-parts 1
                                               :data-part      i))
                 (packed-size   (pb:packed-size fragment))
                 (room          (- max-fragment-size packed-size))
                 (fragment-size (if (< room 5) ;; conservative
                                               ;; estimate of required
                                               ;; number of bytes to
                                               ;; encode a bit of
                                               ;; payload
                                  (error 'insufficient-room
                                         :required  (+ packed-size 5)
                                         :available max-fragment-size)
                                  (min (- room 4) remaining))))

            (setf (notification-data notification)
                  (make-data-fragment wire-data offset fragment-size))
            (incf offset    fragment-size)
            (decf remaining fragment-size)

            (collect fragment :into fragments)
            (while (plusp remaining))
            (finally
             (unless (length= 1 fragments)
               (iter (for fragment in fragments)
                     (setf (fragmented-notification-num-data-parts fragment)
                           (1+ i))))
             (return fragments))))))

;;; Utility functions

(defun make-notification (sequence-number origin
                          &optional
                          scope method wire-schema
                          meta-data timestamps causes)
  "Make and return a `rsb.protocol:notification' instance with SEQUENCE-NUMBER,
ORIGIN and optionally SCOPE, METHOD, WIRE-SCHEMA, META-DATA and
CAUSES."
  (let* ((full?        scope)
         (event-id     (make-instance
                        'event-id
                        :sender-id       (uuid:uuid-to-byte-array origin)
                        :sequence-number sequence-number))
         (meta-data1   (when full?
                         (make-instance
                          'event-meta-data
                          :create-time (timestamp->unix-microseconds
                                        (getf timestamps :create))
                          :send-time   (timestamp->unix-microseconds
                                        (getf timestamps :send)))))
         (notification (apply #'make-instance 'notification
                              :event-id event-id
                              (when full?
                                (list
                                 :scope       (string->bytes (scope-string scope))
                                 :wire-schema (wire-schema->bytes wire-schema)
                                 :meta-data   meta-data1)))))
    (when full?
      ;; Store the method of the event in the new notification if the
      ;; event has one.
      (when method
        (setf (notification-method notification) (keyword->bytes method)))

      ;; Add META-DATA.
      (iter (for (key value) on meta-data :by #'cddr)
            (vector-push-extend
             (make-instance 'user-info
                            :key   (keyword->bytes key)
                            :value (string->bytes value))
             (event-meta-data-user-infos meta-data1)))

      ;; Add TIMESTAMPS.
      (iter (for (key value) on timestamps :by #'cddr)
            ;; Framework timestamps are stored in dedicated fields of
            ;; the notification.
            (unless (member key *framework-timestamps* :test #'eq)
              (vector-push-extend
               (make-instance 'user-time
                              :key       (keyword->bytes key)
                              :timestamp (timestamp->unix-microseconds value))
               (event-meta-data-user-times meta-data1))))

      ;; Add CAUSES.
      (iter (for (origin-id . sequence-number) in causes)
            (vector-push-extend
             (make-instance 'event-id
                            :sender-id       (uuid:uuid-to-byte-array
                                              origin-id)
                            :sequence-number sequence-number)
             (notification-causes notification))))

    notification))

(defun timestamp->unix-microseconds (timestamp)
  "Convert the `local-time:timestamp' instance TIMESTAMP into an
integer which counts the number of microseconds since UNIX epoch."
  (+ (* 1000000 (local-time:timestamp-to-unix timestamp))
     (* 1       (local-time:timestamp-microsecond timestamp))))

(defun unix-microseconds->timestamp (unix-microseconds)
  "Convert UNIX-MICROSECONDS to an instance of
`local-time:timestamp'."
  (let+ (((&values unix-seconds microseconds)
          (floor unix-microseconds 1000000)))
   (local-time:unix-to-timestamp
    unix-seconds :nsec (* 1000 microseconds))))

(defvar *keyword-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :invert)
    readtable)
  "This readtable is used to print and read keywords. The goal is to
get a natural mapping between Lisp keywords and corresponding strings
for most cases.")

(declaim (inline string->bytes bytes->string))

(defun string->bytes (string)
  "Converter STRING into an octet-vector."
  (declare (notinline string->bytes))
  (if (stringp string)
      (sb-ext:string-to-octets string :external-format :ascii)
      (string->bytes (princ-to-string string))))

(defun bytes->string (bytes)
  "Convert BYTES into a string."
  (sb-ext:octets-to-string bytes :external-format :ascii))

(defun keyword->bytes (keyword)
  "Convert the name of KEYWORD into an octet-vector."
  (if (find #\: (symbol-name keyword))
      (string->bytes (symbol-name keyword))
      (let ((*readtable* *keyword-readtable*))
        (string->bytes (princ-to-string keyword)))))

(defun bytes->keyword (bytes)
  "Converter BYTES into a keyword."
  (if (find (char-code #\:) bytes)
      (intern (bytes->string bytes) (find-package :keyword))
      (let ((*package*   (find-package :keyword))
            (*readtable* *keyword-readtable*))
        (read-from-string (bytes->string bytes)))))

(defun wire-schema->bytes (wire-schema)
  "Convert WIRE-SCHEMA to an ASCII representation stored in an
octet-vector."
  (keyword->bytes wire-schema))

(defun bytes->wire-schema (bytes)
  "Return a keyword representing the wire-schema encoded in bytes."
  (when (emptyp bytes)
    (error "~@<Empty wire-schema.~:@>"))
  (bytes->keyword bytes))
