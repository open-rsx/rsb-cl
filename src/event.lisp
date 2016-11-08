;;;; event.lisp --- RSB event class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;; forward declaration
(declaim (special *framework-timestamps*))

(defclass event (scope-mixin
                 plist-meta-data-mixin
                 plist-timestamp-mixin)
  ((sequence-number :initarg  :sequence-number
                    :type     (or null sequence-number)
                    :accessor event-sequence-number
                    :initform nil
                    :documentation
                    "Stores a sequence number of the event \"within\"
the participant sending the event.")
   (id              :type     (or null uuid:uuid)
                    :reader   event-id
                    :writer   (setf event-%id)
                    :documentation
                    "Stores a unique id that is lazily computed from
the unique id of the participant sending the event (stored in the
origin slot) and the sequence number of the event.")
   (scope           :accessor event-scope)
   (origin          :initarg  :origin
                    :type     (or null uuid:uuid)
                    :accessor event-origin
                    :initform nil
                    :documentation
                    "Stores the id of the participant by which the
event was published onto the bus.")
   (method          :initarg  :method
                    :type     (or null keyword)
                    :accessor event-method
                    :initform nil
                    :documentation
                    "Stores the name of a \"method category\" which
characterizes the role of the event in a communication
pattern (examples include \"request\", \"reply\", \"update\").")
   (data            :initarg  :data
                    :type     t
                    :accessor event-data
                    :documentation
                    "Stores the payload of the event as a \"domain
object\" (can be any Lisp object).")
   (meta-data       :accessor event-meta-data)
   (timestamp       :initarg  :timestamps
                    :accessor event-timestamps)
   (causes          :initarg  :causes
                    :type     list
                    :accessor event-causes
                    :initform '()
                    :documentation
                    "Stores a list of `event-id's that identify events
which somehow caused the event."))
  (:default-initargs
   :intern-scope? nil)
  (:documentation
   "Basic unit of information that is exchanged between informers and
listeners. An event is a composite structure consisting of
+ a sequence number
+ an id (derived from the sequence number and the id of the sending
  participant)
+ a scope
+ the id of the participant that sent the event
+ an optional \"method category\"
+ a payload
+ various timestamps
+ optional metadata
+ an optional list of events that caused the event"))

(defmethod shared-initialize :after ((instance   event)
                                     (slot-names t)
                                     &key
                                     (create-timestamp? t))
  ;; Initialize or invalidate id.
  (setf (event-%id instance) nil)

  ;; Maybe set create timestamp.
  (when create-timestamp?
    (setf (timestamp instance :create) (local-time:now))))

(defmethod event-id/opaque ((event event))
  (when-let* ((origin          (event-origin event))
              (sequence-number (event-sequence-number event)))
    (cons origin sequence-number)))

(defmethod event-id :before ((event event))
  (%maybe-set-event-id event))

(defmethod (setf event-sequence-number) :after ((new-value t) (event event))
  (setf (event-%id event) nil))

(defmethod (setf event-origin) :after ((new-value t) (event event))
  (setf (event-%id event) nil))

(defun event= (left right
               &key
               (compare-sequence-numbers? t)
               (compare-origins?          t)
               (compare-methods?          t)
               (compare-timestamps        t)
               (compare-causes?           t)
               (data-test                 #'equal))
  "Return non-nil if the events LEFT and RIGHT are equal.

If COMPARE-SEQUENCE-NUMBERS? is non-nil, return nil unless LEFT and
RIGHT have identical sequence numbers.

If COMPARE-ORIGINS? is non-nil, return nil unless LEFT and RIGHT have
identical origins.

If COMPARE-METHODS? is non-nil, return nil unless LEFT and RIGHT have
identical methods.

COMPARE-TIMESTAMPS can be nil, t or a list of timestamp keys to
compare. If it is t, all timestamps whose keys appear in
`rsb:*framework-timestamps*' are
compared (currently :create, :send, :receive and :deliver).

If COMPARE-CAUSES? is non-nil, return nil unless LEFT and RIGHT have
identical sets of causes.

DATA-TEST has to be a function of two arguments or nil. In the latter
case, the payloads of LEFT and RIGHT are not considered. It is assumed
that DATA-TEST, if supplied, returns non-nil if LEFT and RIGHT are
`eq'."
  (or (eq left right)
      (and (or (not compare-sequence-numbers?)
               (eql (event-sequence-number left)
                    (event-sequence-number right)))
           ;; Scope and origin
           (scope= (event-scope left) (event-scope right))
           ;; Origin
           (or (not compare-origins?)
               (let ((left-origin  (event-origin left))
                     (right-origin (event-origin right)))
                 (or (and (not left-origin) (not right-origin))
                     (and left-origin right-origin
                          (uuid:uuid= left-origin right-origin)))))
           ;; Method
           (or (not compare-methods?)
               (eq (event-method left) (event-method right)))
           ;; Timestamps
           (or (null compare-timestamps)
               (iter (for key in (if (eq compare-timestamps t)
                                     *framework-timestamps*
                                     compare-timestamps))
                     (let ((value-left  (timestamp left key))
                           (value-right (timestamp right key)))
                       (always (or (and (null value-left) (null value-right))
                                   (local-time:timestamp=
                                    value-left value-right))))))
           ;; Causes
           (or (not compare-causes?)
               (set-equal (event-causes left) (event-causes right)
                          :test #'event-id=))
           ;; Payload
           (or (null data-test)
               (funcall data-test (event-data left) (event-data right))))))

(defmethod print-object ((object event) stream)
  (%maybe-set-event-id object) ; force id computation
  (print-unreadable-id-object (object stream :type t)
    (format stream "~@<~@[~A ~]~A ~:/rsb::print-event-data/~@:>"
            (event-method object)
            (scope-string (event-scope object))
            (event-data object))))

;;; Construction

(declaim (inline %check-event-timestamp %check-event-meta-data
                 %check-event-cause %check-event-arguments))

(defun %check-event-timestamp (key value)
 (unless (typep value 'local-time:timestamp)
   (error 'simple-type-error
          :datum            value
          :expected-type    'local-time:timestamp
          :format-control   "~@<The value ~S supplied for timestamp ~
                             key ~S is not a ~S.~@:>"
          :format-arguments (list value key 'local-time:timestamp))))

(defun %check-event-meta-data (key value)
  (unless (typep value '(or string keyword real))
    (error 'simple-type-error
           :datum            value
           :expected-type    '(or string keyword real)
           :format-control   "~@<The value ~S supplied for meta-data key ~
                              ~S is not of type ~S.~@:>"
           :format-arguments (list value key '(or string keyword real)))))

(defun %check-event-cause (value)
  (unless (typep value 'event-id)
    (error 'simple-type-error
           :datum            value
           :expected-type    'event-id
           :format-control   "~@<The value ~S as cause is not of type ~
                              ~S.~@:>"
           :format-arguments (list value 'event-id))))

(defun %check-event-arguments (timestamps meta-data causes)
  ;; Check timestamps.
  (iter (for (key value) on timestamps :by #'cddr)
        (%check-event-timestamp key value))

  ;; Check meta-data items.
  (iter (for (key value) on meta-data :by #'cddr)
        (%check-event-meta-data key value))

  ;; Check causes.
  (mapc #'%check-event-cause causes))

(defun make-event (scope data
                   &rest meta-data
                   &key
                   method
                   causes
                   timestamps
                   (create-timestamp? t)
                   unchecked?
                   &allow-other-keys)
  "Construct an event addressed at SCOPE with payload DATA and,
   optionally, meta-data consisting of the keys and values in the
   plist META-DATA.

   CAUSES can be used to supply a list of causes.

   TIMESTAMPS is an alist with elements of the form

     (KEY . TIMESTAMP)

   where TIMESTAMP is a `local-time:timestamp' instance.

   CREATE-TIMESTAMP? controls whether a :create timestamp for the
   current time is added to the event."
  (let ((meta-data (remove-from-plist
                    meta-data :method :causes :timestamps
                              :create-timestamp? :unchecked?)))
    (unless unchecked?
      (%check-event-arguments timestamps meta-data causes))
    (make-instance 'event
                   :scope             (make-scope scope)
                   :method            method
                   :data              data
                   :meta-data         meta-data
                   :causes            causes
                   :timestamp         timestamps
                   :create-timestamp? create-timestamp?)))

;;; Utility functions

(declaim (ftype (function (event-id event-id) t) event-id=))

(defun event-id= (left right)
  "Return non-nil if the event ids LEFT and RIGHT refer to the same
event."
  (or (eq left right)
      (and (= (cdr left) (cdr right))
           (uuid:uuid= (car left) (car right)))))

(declaim (ftype (function (event-id) (values uuid:uuid &optional))
                event-id->uuid)
         (inline event-id->uuid))
(defun event-id->uuid (event-id)
  "Derive a UUID from EVENT-ID."
  (uuid:make-v5-uuid
   (car event-id) (format nil "~(~8,'0X~)" (cdr event-id))))

(defun %maybe-set-event-id (event)
  "When the id slot of EVENT is nil compute a unique id based on the
origin id of EVENT and the sequence number of EVENT. If EVENT does not
have an origin, do nothing."
  (unless (slot-value event 'id)
    (when-let ((id (event-id/opaque event)))
      (setf (event-%id event) (event-id->uuid id)))))

(defun print-event-data (stream data &optional colon? at?)
  "Print the event payload DATA to stream in a type-dependent manner.
This function is intended for use with the / `format' control.

COLON? controls whether an indication of the size of DATA should be
printed.

AT? is ignored."
  (declare (ignore at?))
  (let+ ((*print-length* (or *print-length* 5))
         ((&flet maybe-shorten-string (string max)
            ;; Shorten STRING to obey `*print-length*'.
            (let ((length (length string)))
              (if (<= length max)
                  string
                  (concatenate
                   'string (subseq string 0 max) "...")))))
         ((&flet make-printable (string)
            ;; Replace unprintable characters in STRING with ".".
            (substitute-if
             #\. (lambda (char) (< (char-code char) 32)) string)))
         ((&flet print-string (string)
            (make-printable (maybe-shorten-string string *print-length*)))))
    (format stream "~:[~A~:;~S~]~@[ (~D)~]"
            (stringp data)
            (etypecase data
              (string (print-string data))
              (scope  (print-string (scope-string data)))
              (t      data))
            (when (and colon? (typep data 'sequence))
              (length data)))))
