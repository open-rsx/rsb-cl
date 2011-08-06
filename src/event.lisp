;;; event.lisp --- RSB event class.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb)

(define-plist-data-mixin meta-data)
(define-plist-data-mixin timestamp)

(defclass event (scope-mixin
		 plist-meta-data-mixin
		 plist-timestamp-mixin)
  ((sequence-number :initarg  :sequence-number
		    :type     sequence-number
		    :accessor event-sequence-number
		    :documentation
		    "Stores a sequence number of the event \"within\"
the participant sending the event.")
   (id              :type     (or null uuid:uuid)
		    :reader   event-id
		    :writer   (setf %event-id)
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
   (type            :initarg  :type
		    :type     (or symbol list)
		    :accessor event-type
		    :initform t ;; TODO could also use (type-of data)
		    :documentation
		    "Note: in most cases, the type can be inferred
from the contents of the data slot. This slot allows the data to
treated under the assumption of it being of a certain type. The most
common use case probably is forcing a more general type.")
   (data            :initarg  :data
		    :type     t
		    :accessor event-data
		    :documentation
		    "")
   (meta-data       :accessor event-meta-data)
   (timestamp       :accessor event-timestamps))
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
+ a type describing the payload
+ various timestamps
+ optional metadata"))

(defmethod shared-initialize :before ((instance   event)
				      (slot-names t)
				      &key
				      (type nil type-supplied?)
				      (data nil data-supplied?)) ;; fail when these are missing?
  (when (and type-supplied? data-supplied?
	     (not (typep data type)))
    (error 'type-error
	   :datum         data
	   :expected-type type)))

(defmethod shared-initialize :after ((instance   event)
                                     (slot-names t)
                                     &key
				     (create-timestamp? t))
  ;; Initialize or invalidate id.
  (setf (%event-id instance) nil)

  ;; Maybe set create timestamp.
  (when create-timestamp?
    (setf (timestamp instance :create) (local-time:now))))

(defmethod event-id :before ((event event))
  (%maybe-set-event-id event))

(defmethod (setf event-sequence-number) :after ((new-value t) (event event))
  (setf (%event-id event) nil))

(defmethod (setf event-origin) :after ((new-value t) (event event))
  (setf (%event-id event) nil))

(defun event= (left right
	       &key
	       (compare-sequence-numbers? t)
	       (compare-origins?          t)
	       (compare-methods?          t)
	       (compare-timestamps        t)
	       (data-test                 #'equal))
  "Return non-nil if the events LEFT and RIGHT are equal.
If COMPARE-SEQUENCE-NUMBERS? is non-nil, return nil unless LEFT and
RIGHT have identical sequence numbers.
If COMPARE-ORIGINS? is non-nil, return nil unless LEFT and RIGHT have
identical origins.
if COMPARE-METHODS? is non-nil, return nil unless LEFT and RIGHT have
identical methods.
COMPARE-TIMESTAMPS can be nil, t or a list of timestamp keys to
compare. If it is t, all RSB timestamps are
compared (currently :create, :send, :receive and :deliver).
DATA-TEST has to be a function of two arguments or nil. In the latter
case, the payloads of LEFT and RIGHT are not considered."
  (and (or (not compare-sequence-numbers?)
	   (= (event-sequence-number left)
	      (event-sequence-number right)))
       ;; Scope and origin
       (scope= (event-scope left) (event-scope right))
       ;; Origin
       (or (not compare-origins?)
	   (let ((left-origin  (event-origin left))
		 (right-origin (event-origin right)))
	     (or (and (not left-origin) (not right-origin))
		 (and left-origin right-origin
		      (uuid= left-origin right-origin)))))
       ;; Method
       (or (not compare-methods?)
	   (eq (event-method left) (event-method right)))
       ;; Timestamps
       (or (null compare-timestamps)
	   (iter (for key in (if (eq compare-timestamps t)
				 '(:create :send :receive :deliver)
				 compare-timestamps))
		 (let ((value-left  (timestamp left key))
		       (value-right (timestamp right key)))
		   (always (or (and (null value-left) (null value-right))
			       (local-time:timestamp=
				value-left value-right))))))
       ;; Data and type
       (type= (event-type left) (event-type right))
       (or (null data-test)
	   (funcall data-test (event-data left) (event-data right)))))

(defmethod print-object ((object event) stream)
  (%maybe-set-event-id object) ;; force id computation
  (print-unreadable-id-object (object stream :type t)
    (format stream "~@<~@[~A ~]~A ~S ~/rsb::print-event-data/~@:>"
	    (event-method object)
	    (scope-string (event-scope object))
	    (event-type object)
	    (event-data object))))


;;; Convenience
;;

(defun make-event (scope data
		   &rest meta-data
		   &key
		   method
		   &allow-other-keys)
  "Construct an event addressed at SCOPE with payload DATA and,
optionally, meta-data consisting of the keys and values in the plist
META-DATA."
  (make-instance 'event
		 :scope     (make-scope scope)
		 :method    method
		 :data      data
		 :meta-data (remove-from-plist meta-data :method)))

(defun make-event/typed (scope data type
			 &rest meta-data
			 &key
			 method
			 &allow-other-keys)
  "Construct an event addressed at SCOPE with payload DATA and type TYPE.
Optionally, add meta-data consisting of the keys and values in the
plist META-DATA."
  (make-instance 'event
		 :scope     (make-scope scope)
		 :method    method
		 :data      data
		 :type      type
		 :meta-data (remove-from-plist meta-data :method)))


;;; Utility functions
;;

(declaim (inline %maybe-set-event-id))

(defun %maybe-set-event-id (event)
  "When the id slot of EVENT is nil compute a unique id based on the
origin id of EVENT and the sequence number of EVENT. If EVENT does not
have an origin, do nothing."
  (when (and (not (slot-value event 'id)) (event-origin event))
    (setf (%event-id event)
	  (uuid:make-v5-uuid
	   (event-origin event)
	   (format nil "~(~8,'0X~)" (event-sequence-number event))))))

(defun print-event-data (stream data colon? at?)
  "Print the event payload DATA to stream in a type-dependent manner.
This function is intended for use with the / `format' control. COLON?
and AT? are ignored."
  (declare (ignore colon? at?))
  (let ((*print-length* (or *print-length* 5)))
    (format stream "~S~@[ (~D)~]"
	    (etypecase data
	      (string (%maybe-shorten-string data *print-length*))
	      (t      data))
	    (when (typep data 'sequence)
	      (length data)))))

(defun %maybe-shorten-string (string max)
  "DOC"
  (let ((length (length string)))
    (if (<= length max)
	string
	(concatenate 'string
		     (subseq string 0 (min max length))
		     "..."))))
