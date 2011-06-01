;;; conversion.lisp ---
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

(in-package :rsb.transport.spread)


;;; Notification -> Event
;;

(defmethod notification->event ((notification t) (pool assembly-pool))
  "Try to convert NOTIFICATION into an event. This may not be possible
in a single step since NOTIFICATION can be a part of an event that has
been fragmented into multiple notifications."
  (if (<= (rsb.protocol::notification-num-data-parts notification) 1)
      ;; When the event has been transmitted as a single notification,
      ;; an assembly step is not required.
      (one-notification->event notification)
      ;; When the event has been fragmented into multiple
      ;; notifications, try to assemble for each
      ;; notification. `merge-fragment' returns nil until all
      ;; fragments have arrived.
      (let ((assembly (merge-fragment pool notification)))
	(when assembly
	  (one-notification->event
	   (aref (assembly-fragments assembly) 0)
	   (assembly-concatenated-data assembly))))))

(defun one-notification->event (notification &optional data)
  "DOC"
  (bind (((:accessors-r/o
	   (id          rsb.protocol::notification-id)
	   (scope       rsb.protocol::notification-scope)
	   (wire-schema rsb.protocol::notification-wire-schema)
	   (payload     rsb.protocol::notification-data)
	   (meta-data   rsb.protocol::notification-meta-data)) notification)
	 (event))

    (when (emptyp id)
      (error "Notification ~S does not have an id." ;; TODO condition; maybe communication-error or protocol-error?
	     notification))

    (setf event (make-instance
		 'rsb:event
		 :id                (uuid:byte-array-to-uuid id)
		 :scope             (make-scope (sb-ext:octets-to-string
						 scope :external-format :ascii))
		 :type              t ;(pb::proto-type-name->lisp-type-symbol
					;wire-schema)
		 :data              (wire-data->event-data
				     (or data payload)
				     (sb-ext:octets-to-string
				      wire-schema :external-format :ascii))
		 :create-timestamp? nil))

    ;; Sender and timestamps TODO should these really be optional?
    (when meta-data
      ;; "User infos"
      (iter (for user-info in-vector (rsb.protocol::meta-data-user-infos meta-data))
	    (setf (meta-data event (rsb.protocol::user-info-key user-info))
		  (rsb.protocol::user-info-value user-info)))

      ;; Set origin, if present
      (unless (emptyp (rsb.protocol::meta-data-sender-id meta-data))
	(setf (event-origin event)
	      (uuid:byte-array-to-uuid
	       (rsb.protocol::meta-data-sender-id meta-data))))
      ;; Set :create timestamp, if present
      (unless (zerop (rsb.protocol::meta-data-create-time meta-data))
	(setf (timestamp event :create)
	      (unix-microseconds->timestamp
	       (rsb.protocol::meta-data-create-time meta-data))))
      ;; Set :send timestamp, if present
      (unless (zerop (rsb.protocol::meta-data-send-time meta-data))
	(setf (timestamp event :send)
	      (unix-microseconds->timestamp
	       (rsb.protocol::meta-data-send-time meta-data))))
      ;; Set :receive timestamp
      (setf (timestamp event :receive) (local-time:now))

      (iter (for user-time in-vector (rsb.protocol::meta-data-user-times meta-data))
	    (setf (timestamp event (make-keyword (sb-ext:octets-to-string
						  (rsb.protocol::user-time-key user-time)
						  :external-format :ascii)))
		  (unix-microseconds->timestamp
		   (rsb.protocol::user-time-timestamp user-time)))))

    event))


;;; Event -> Notification
;;

(defun event->notifications (event max-fragment-size)
  "Convert EVENT into one or more notifications. More than one
notification is required when data contained in event does not fit
into one notification."
  (bind (((:accessors-r/o
	   (id event-id) (scope event-scope) (origin event-origin)
	   (data       event-data)
	   (meta-data  event-meta-data)
	   (timestamps event-timestamps)) event)
	 (data1 (event-data->wire-data data)))
    (if (> (length data1) max-fragment-size)
	;; Split DATA1 into multiple fragment and make a notification
	;; for each fragment.
	(bind ((fragments     (fragment-data data1 max-fragment-size))
	       (num-fragments (length fragments)))
	  (iter (for fragment in    fragments)
		(for i        :from 0)
		(collect
		    (make-notification id scope origin
				       "string" fragment
				       :meta-data      meta-data
				       :timestamps     timestamps
				       :data-part      i
				       :num-data-parts num-fragments))))
	;; DATA1 fits into a single notification.
	(list
	 (make-notification id scope origin
			    "string" data1
			    :meta-data meta-data
			    :timestamps timestamps)))))


;;; Data Conversion
;;

(defun wire-data->event-data (data wire-schema)
  "DOC"
  (bind (((:values mechanism designator) (decode-wire-schema wire-schema)))
    (cond
      ((eq mechanism :fundamental)
       (cond
	 ((string= designator "string")
	  (sb-ext:octets-to-string data)))) ;; TODO make a "fundamental" converter for this?

      ((eq mechanism :protocol-buffer)
       (rsb.converter:wire->domain :protocol-buffer data designator))
      (t
       (error "Unsupported wire-type/-schema (~S,~S)" mechanism designator)))))

(defun event-data->wire-data (data)
  "DOC"
  (typecase data
    (octet-vector data)
    (string       (sb-ext:string-to-octets data))
    (t            (rsb.converter:domain->wire :protocol-buffer data))))


;;; Utility functions
;;

(defun make-notification (id scope origin wire-schema data
			  &key
			  meta-data
			  timestamps
			  (num-data-parts 1)
			  (data-part      0))
  "Make a `rsb.protocol:notification' instance with ID, SCOPE,
WIRE-SCHEMA, DATA and optionally META-DATA. When NUM-DATA-PARTS and
DATA-PART are not supplied, values that indicate a non-fragmented
notification are chosen."
  (bind ((meta-data1   (make-instance
			'rsb.protocol::meta-data
			:sender-id   (uuid:uuid-to-byte-array origin)
			:create-time (timestamp->unix-microseconds
				      (getf timestamps :create))
			:send-time   (timestamp->unix-microseconds
				      (local-time:now))))
	 (notification (make-instance
			'rsb.protocol::notification
			:id             (uuid:uuid-to-byte-array id)
			:scope          (sb-ext:string-to-octets
					 (scope-string scope)
					 :external-format :ascii)
			:wire-schema    (sb-ext:string-to-octets
					 wire-schema
					 :external-format :ascii)
			:num-data-parts num-data-parts
			:data-part      data-part
			:data           data
			:meta-data      meta-data1)))

    ;; Add META-DATA.
    (iter (for (key value) on meta-data :by #'cddr)
	  (vector-push-extend
	   (make-instance 'rsb.protocol::user-info
			  :key   (string key)
			  :value (prin1-to-string value))
	   (rsb.protocol::meta-data-user-infos meta-data1)))

    ;; Add TIMESTAMPS.
    (iter (for (key value) on timestamps :by #'cddr)
	  (unless (eq key :create) ;; the event should not have :send,
				   ;; :receive or :deliver at this
				   ;; point
	    (let ((name (sb-ext:string-to-octets
			 (string key)
			 :external-format :ascii))
		  (flat (timestamp->unix-microseconds value)))
	      (vector-push-extend
	       (make-instance 'rsb.protocol::user-time
			      :key       name
			      :timestamp flat)
	       (rsb.protocol::meta-data-user-times meta-data1)))))

    ;; Return the complete notification instance.
    notification))

(defun timestamp->unix-microseconds (timestamp)
  "Convert the `local-time:timestamp' instance TIMESTAMP into an
integer which counts the number of microseconds since UNIX epoch."
  (nth-value
   0 (+ (* 1000000 (local-time:timestamp-to-unix timestamp))
	(* 1       (local-time:timestamp-microsecond timestamp)))))

(defun unix-microseconds->timestamp (unix-microseconds)
  "Convert UNIX-MICROSECONDS to an instance of
`local-time:timestamp'."
  (bind (((:values unix-seconds microseconds)
	  (floor unix-microseconds 1000000)))
   (local-time:unix-to-timestamp
    unix-seconds :nsec (* 1000 microseconds))))

;; TODO would not belong in spread transport, if we really did this
(defun decode-wire-schema (wire-schema)
  "DOC"
  (let ((colon (position #\: wire-schema)))
    (if colon
	(values
	 (make-keyword (string-upcase (subseq wire-schema 0 colon))) ;; TODO make a function?
	 (subseq wire-schema (1+ colon)))
	(values
	 :fundamental
	 wire-schema))))

;; A funny piece of meta-data
;; :key   "descriptor"
;; :value (base64:usb8-array-to-base64-string
;;	(pb::pack1 (pb::descriptor data))))
