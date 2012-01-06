;;; event.lisp --- RSB event class.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(in-package :rsb)

(define-plist-data-mixin meta-data)
(define-plist-data-mixin timestamp)

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
   (timestamp       :accessor event-timestamps)
   (causes          :initarg  :causes
		    :type     list
		    :accessor event-causes
		    :initform nil
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
+ a type describing the payload
+ various timestamps
+ optional metadata
+ an optional list of events that caused the event"))

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

(defmethod event-id/opaque ((event event))
  (when-let* ((origin          (event-origin event))
	      (sequence-number (event-sequence-number event)))
    (cons origin sequence-number)))

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
compare. If it is t, all RSB timestamps are
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
	   ;; Causes
	   (or (not compare-causes?)
	       (set-equal (event-causes left) (event-causes right)
			  :test #'event-id=))
	   ;; Data and type
	   (type= (event-type left) (event-type right))
	   (or (null data-test)
	       (funcall data-test (event-data left) (event-data right))))))

(defmethod print-object ((object event) stream)
  (%maybe-set-event-id object) ;; force id computation
  (print-unreadable-id-object (object stream :type t)
    (format stream "~@<~@[~A ~]~A ~S ~:/rsb::print-event-data/~@:>"
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
		   causes
		   &allow-other-keys)
  "Construct an event addressed at SCOPE with payload DATA and,
optionally, meta-data consisting of the keys and values in the plist
META-DATA.
CAUSES can be used to supply a list of causes."
  (make-instance 'event
		 :scope     (make-scope scope)
		 :method    method
		 :data      data
		 :meta-data (remove-from-plist meta-data :method :causes)
		 :causes    causes))

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

(declaim (ftype (function (event-id event-id) t) event-id=))

(defun event-id= (left right)
  "Return non-nil if the event ids LEFT and RIGHT refer to the same
event."
  (and (= (cdr left) (cdr right))
       (uuid:uuid= (car left) (car right))))

(declaim (ftype (function (event-id) uuid:uuid) event-id->uuid)
	 (inline event-id->uuid))

(defun event-id->uuid (event-id)
  "Derive a UUID from EVENT-ID."
  (uuid:make-v5-uuid
   (car event-id) (format nil "~(~8,'0X~)" (cdr event-id))))

(declaim (inline %maybe-set-event-id))

(defun %maybe-set-event-id (event)
  "When the id slot of EVENT is nil compute a unique id based on the
origin id of EVENT and the sequence number of EVENT. If EVENT does not
have an origin, do nothing."
  (unless (slot-value event 'id)
    (when-let ((id (event-id/opaque event)))
      (setf (%event-id event) (event-id->uuid id)))))

(defun print-event-data (stream data colon? at?)
  "Print the event payload DATA to stream in a type-dependent manner.
This function is intended for use with the / `format' control.
COLON? controls whether an indication of the size of DATA should be
printed.
AT? is ignored."
  (declare (ignore at?))
  (let ((*print-length* (or *print-length* 5)))
    (format stream "~S~@[ (~D)~]"
	    (etypecase data
	      (string (%maybe-shorten-string data *print-length*))
	      (t      data))
	    (when (and colon? (typep data 'sequence))
	      (length data)))))

(defun %maybe-shorten-string (string max)
  "DOC"
  (let ((length (length string)))
    (if (<= length max)
	string
	(concatenate 'string
		     (subseq string 0 (min max length))
		     "..."))))
