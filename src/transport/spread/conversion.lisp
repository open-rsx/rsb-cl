;;;; conversion.lisp --- Event <-> notification conversion for Spread transport.
;;;;
;;;; Copyright (C) 2011-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; Utility functions

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

(declaim (inline string->bytes bytes->string))

(defun string->bytes (string)
  "Convert STRING into an octet-vector."
  (declare (notinline string->bytes))
  (if (stringp string)
      (sb-ext:string-to-octets string :external-format :ascii)
      (string->bytes (princ-to-string string))))

(defun bytes->string (bytes)
  "Convert BYTES into a string."
  (sb-ext:octets-to-string bytes :external-format :ascii))

(defvar *keyword-readtable*
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :invert)
    readtable)
  "This readtable is used to print and read keywords.

   The goal is to get a natural mapping between Lisp keywords and
   corresponding strings for most cases.")

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

;;; Notification -> Event

(defun assemble-notification (pool notification)
  "Maybe assemble the `fragmented-notification' NOTIFICATION using POLL.

   Return assembled `notification' instance or `nil'."
  (declare (type fragmented-notification notification))
  (if (<= (fragmented-notification-num-data-parts notification) 1)
      ;; When the event has been transmitted as a single notification,
      ;; an assembly step is not required.
      (let* ((notification (fragmented-notification-notification notification))
             (data         (notification-data notification)))
        (make-incoming-notification notification data))
      ;; When the event has been fragmented into multiple
      ;; notifications, try to assemble for each
      ;; notification. `merge-fragment' returns nil until all
      ;; fragments have arrived.
      (when-let* ((assembly       (merge-fragment pool notification))
                  (first-fragment (aref (assembly-fragments assembly) 0))
                  (data           (assembly-concatenated-data assembly)))
        (make-incoming-notification
         (fragmented-notification-notification first-fragment)
         data))))

(defun one-notification->event (converter notification data
                                &key
                                expose-wire-schema?
                                expose-payload-size?)
  "Convert NOTIFICATION to an `event' using CONVERTER for DATA.

   Return the decoded event."
  (let+ (((&flet event-id->cons (event-id)
            (cons (uuid:byte-array-to-uuid (event-id-sender-id event-id))
                  (event-id-sequence-number event-id))))
         ((&accessors-r/o
           (scope       notification-scope)
           (event-id    notification-event-id)
           (method      notification-method)
           (wire-schema notification-wire-schema)
           (meta-data   notification-meta-data)
           (causes      notification-causes))
          notification)
         ((&accessors-r/o
           (sender-id       event-id-sender-id)
           (sequence-number event-id-sequence-number))
          event-id)
         (wire-schema (bytes->wire-schema wire-schema))
         (data*       (rsb.converter:wire->domain
                       converter data wire-schema))
         (event       (make-instance
                       'rsb:event
                       :origin            (uuid:byte-array-to-uuid sender-id)
                       :sequence-number   sequence-number
                       :scope             (make-scope (bytes->string scope))
                       :method            (unless (emptyp method)
                                            (bytes->keyword method))
                       :causes            (map 'list #'event-id->cons causes)
                       :data              data*
                       :create-timestamp? nil))
         ((&flet set-timestamp (key value)
            (unless (zerop value)
              (setf (timestamp event key)
                    (unix-microseconds->timestamp value))))))

    ;; "User infos" and timestamps
    (when meta-data
      ;; "User infos"
      (iter (for user-info in-vector (event-meta-data-user-infos meta-data))
            (setf (rsb:meta-data event (bytes->keyword
                                        (user-info-key user-info)))
                  (bytes->string (user-info-value user-info))))

      ;; Set framework timestamps :create, :send, :receive and
      ;; :deliver, if present.
      (set-timestamp :create  (event-meta-data-create-time meta-data))
      (set-timestamp :send    (event-meta-data-send-time meta-data))
      (set-timestamp :receive (event-meta-data-receive-time meta-data))
      (set-timestamp :deliver (event-meta-data-deliver-time meta-data))
      ;; Set "user timestamps"
      (iter (for user-time in-vector (event-meta-data-user-times meta-data))
            (set-timestamp (bytes->keyword (user-time-key user-time))
                           (user-time-timestamp user-time))))

    ;; When requested, store transport metrics as meta-data items.
    (when expose-wire-schema?
      (setf (rsb:meta-data event :rsb.transport.wire-schema) wire-schema))
    (when expose-payload-size?
      (setf (rsb:meta-data event :rsb.transport.payload-size) (length data)))

    event))

;;; Event -> Notification

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-notification (sequence-number origin
                            &optional
                            scope method wire-schema
                            meta-data timestamps causes)
    "Make and return a `rsb.protocol:notification' instance with SEQUENCE-NUMBER,
     ORIGIN and optionally SCOPE, METHOD, WIRE-SCHEMA, META-DATA and
     CAUSES."
    (let+ ((full?     scope)
           (event-id (make-instance 'event-id
                                    :sender-id       (uuid:uuid-to-byte-array origin)
                                    :sequence-number sequence-number))
           ((&flet make-meta-data ()
              (let ((result (make-instance 'event-meta-data)))
                ;; Add META-DATA.
                (iter (for (key value) on meta-data :by #'cddr)
                  (vector-push-extend
                   (make-instance 'user-info
                                  :key   (keyword->bytes key)
                                  :value (string->bytes value))
                   (event-meta-data-user-infos result)))

                ;; Add framework timestamps in TIMESTAMPS.
                (macrolet
                    ((set-timestamp (which accessor)
                       `(when-let ((value (getf timestamps ,which)))
                          (setf (,accessor result)
                                (timestamp->unix-microseconds value)))))
                  (set-timestamp :create  event-meta-data-create-time)
                  (set-timestamp :send    event-meta-data-send-time)
                  (set-timestamp :receive event-meta-data-receive-time)
                  (set-timestamp :deliver event-meta-data-deliver-time))
                ;; Add "user timestamps" in TIMESTAMPS.
                (iter (for (key value) on timestamps :by #'cddr)
                  ;; Framework timestamps are stored in dedicated fields of
                  ;; the notification.
                  (unless (member key *framework-timestamps* :test #'eq)
                    (vector-push-extend
                     (make-instance 'user-time
                                    :key       (keyword->bytes key)
                                    :timestamp (timestamp->unix-microseconds value))
                     (event-meta-data-user-times result))))
                result)))
           (notification (if full?
                             (make-instance 'notification
                                            :event-id    event-id
                                            :scope       (string->bytes (scope-string scope))
                                            :wire-schema (wire-schema->bytes wire-schema)
                                            :meta-data   (make-meta-data))
                             (make-instance 'notification :event-id event-id))))
      (when full?
        ;; Store the method of the event in the new notification if the
        ;; event has one.
        (when method
          (setf (notification-method notification) (keyword->bytes method)))

        ;; Add CAUSES.
        (iter (for (origin-id . sequence-number) in causes)
          (vector-push-extend
           (make-instance 'event-id
                          :sender-id       (uuid:uuid-to-byte-array
                                            origin-id)
                          :sequence-number sequence-number)
           (notification-causes notification))))

      notification)))

(declaim (type message-length-limit *max-other-fragment-overhead*
               *max-payload-overhead*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((max-fragments       16383)
         (notification        (make-notification 0 (uuid:make-null-uuid)))
         (fragment            (make-instance 'fragmented-notification
                                             :notification   notification
                                             :num-data-parts max-fragments
                                             :data-part      max-fragments))
         (no-payload-overhead (pb:packed-size fragment))
         (fragment-overhead   (progn
                                (setf (notification-data notification)
                                      (nibbles:make-octet-vector
                                       network.spread:+maximum-message-data-length+))
                                (- (pb:packed-size fragment)
                                   network.spread:+maximum-message-data-length+))))

    (defparameter *max-other-fragment-overhead*
      fragment-overhead
      "Maximum overhead in octets for non-first fragments.")

    (defparameter *max-payload-overhead*
      (- fragment-overhead no-payload-overhead)
      "Maximum overhead in octets added by the payload.")))

(declaim (ftype (function (notification simple-octet-vector message-length-limit)
                          (values function &optional))
                split-notification))
(defun split-notification (notification wire-data max-fragment-size)
  (let ((event-id                    (notification-event-id notification))
        (data-size                   (length wire-data))
        (offset                      0)
        (i                           0)
        (num-parts                   0)
        (other-fragment-payload-size (- max-fragment-size
                                        *max-other-fragment-overhead*)))
    (declare (type array-index offset i num-parts))
    (flet ((make-fragment ()
             (unless (or (zerop i) (< i num-parts))
               (return-from make-fragment))
             (let* ((notification  (if (zerop i)
                                       notification
                                       (make-instance 'notification
                                                      :event-id event-id)))
                    (fragment      (make-instance 'fragmented-notification
                                                  :notification   notification
                                                  :num-data-parts num-parts
                                                  :data-part      i))
                    (payload-size  (if (zerop i)
                                       (- max-fragment-size
                                          (the message-length-limit
                                               (pb:packed-size fragment))
                                          *max-payload-overhead*)
                                       other-fragment-payload-size))
                    (payload-size  (min payload-size (- data-size offset))))
               (when (minusp payload-size)
                 (error 'insufficient-room
                        :available max-fragment-size
                        :required  (+ (the message-length-limit
                                           (pb:packed-size fragment))
                                      *max-payload-overhead*)))
               ;; After constructing the initial fragment, calculate
               ;; the required number of fragments.
               (when (zerop i)
                 (setf num-parts
                       (1+ (ceiling (- data-size payload-size)
                                    other-fragment-payload-size))
                       (fragmented-notification-num-data-parts fragment)
                       num-parts))
               ;; Stores slice of WIRE-DATA as payload for the fragment.
               (setf (notification-data notification)
                     (subseq wire-data offset (+ offset payload-size)))
               (incf offset payload-size)
               (incf i)
               (values fragment (1- i) num-parts))))
      #'make-fragment)))
