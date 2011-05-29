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

(defclass event (uuid-mixin
		 scope-mixin
		 plist-meta-data-mixin
		 plist-timestamp-mixin)
  ((id        :accessor event-id)
   (scope     :accessor event-scope)
   (origin    :initarg  :origin
	      :type     uuid:uuid
	      :accessor event-origin
	      :documentation
	      "Stores the id of the participant by which the event was
published onto the bus.")
   (type      :initarg  :type
	      :type     (or symbol list)
	      :accessor event-type
	      :initform t ;; TODO could also use (type-of data)
	      :documentation
	      "Note: in most cases, the can be inferred from the
contents of the data slot. This slot allows the data to treated under
the assumption of it being of a certain type. The most common use case
probably is forcing a more general type.")
   (data      :initarg  :data
	      :type     t
	      :accessor event-data
	      :documentation
	      "")
   (meta-data :accessor event-meta-data)
   (timestamp :accessor event-timestamps))
  (:documentation
   "Basic unit of information that is exchanged between informers and
listeners. An event is a composite structure consisting of
+ an id
+ a scope
+ the id of the participant that sent the event
+ a payload
+ a type describing the payload
+ optional metadata"))

(defmethod initialize-instance :before ((instance event)
					&key
					(type nil type-supplied?)
					(data nil data-supplied?)) ;; fail when these are missing?
  (when (and type-supplied? data-supplied?
	     (not (typep data type)))
    (error 'type-error
	   :datum         data
	   :expected-type type)))

(defun event= (left right
	       &key
	       (compare-ids?     t)
	       (compare-origins? t)
	       (data-test        #'equal))
  "Return non-nil if the events LEFT and RIGHT are equal.
If COMPARE-IDS? is non-nil, return nil unless LEFT and RIGHT have
identical ids.
If COMPARE-ORIGINS? is non-nil, return nil unless LEFT and RIGHT have
identical origins.
DATA-TEST has to be a function of two arguments or nil. In the latter
case, the payloads of LEFT and RIGHT are not considered."
  (and (or (not compare-ids?)
	   (uuid= (event-id left) (event-id right)))
       (scope= (event-scope left) (event-scope right))
       (or (not compare-origins?)
	   (uuid= (event-origin left) (event-origin right)))
       (type= (event-type left) (event-type right))
       (or (null data-test)
	   (funcall data-test (event-data left) (event-data right)))))

(defmethod print-object ((object event) stream)
  (bind (((:slots scope type data) object)
	 (*print-length* (or *print-length* 5)))
    (print-unreadable-id-object (object stream :type t)
      (format stream "~@<~A ~S ~S~@[ (~D)~]~@:>"
	      (scope-string scope)
	      type
	      (etypecase data
		(string (%maybe-shorten-string data *print-length*))
		(t      data))
	      (when (typep data 'sequence)
		(length data))))))


;;; Convenience
;;

(defun make-event (scope data
		   &rest meta-data
		   &key &allow-other-keys)
  "Construct an event addressed at SCOPE with payload DATA and,
optionally, meta-data consisting of the keys and values in the plist
META-DATA."
  (make-instance 'event
		 :scope     (make-scope scope)
		 :data      data
		 :meta-data meta-data))

(defun make-event/typed (scope data type
			 &rest meta-data
			 &key &allow-other-keys)
  "Construct an event addressed at SCOPE with payload DATA and type TYPE.
Optionally, add meta-data consisting of the keys and values in the
plist META-DATA."
  (make-instance 'event
		 :scope     (make-scope scope)
		 :data      data
		 :type      type
		 :meta-data meta-data))


;;; Utility functions
;;

(defun %maybe-shorten-string (string max)
  "DOC"
  (let ((length (length string)))
    (if (<= length max)
	string
	(concatenate 'string
		     (subseq string 0 (min max length))
		     "..."))))
