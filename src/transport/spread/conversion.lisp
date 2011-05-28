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
  (if (zerop (rsb.protocol::notification-num-data-parts notification))
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
	   (attachment  rsb.protocol::notification-data)
	   (meta-info   rsb.protocol::notification-meta-infos)) notification)
	 (event))

    (when (emptyp id)
      (error "Notification ~S does not have an id." ;; TODO condition; maybe communication-error or protocol-error?
	     notification))

    (setf event (make-instance 'rsb:event
			       :id    (uuid:make-uuid-from-string id)
			       :scope (make-scope scope)
			       :type  t ;(pb::proto-type-name->lisp-type-symbol
					;wire-schema)
			       :data  (wire-data->event-data
				       (or data (rsb.protocol::attachment-binary attachment))
				       wire-schema)))

    ;; Meta-data
    (iter (for meta-data in-vector meta-info)
	  (setf (meta-data event ;(read-from-string
				  (rsb.protocol::meta-info-key meta-data));)

		(rsb.protocol::meta-info-value meta-data)))

    event))


;;; Event -> Notification
;;

(defun event->notifications (event max-fragment-size)
  "Convert EVENT into one or more notifications. More than one
notification is required when data contained in event does not fit
into one notification."
  (bind (((:accessors-r/o
	   (scope event-scope) (id event-id)
	   (data event-data) (meta-data event-meta-data)) event)
	 (id1    (format nil "~A" id))
	 (scope1 (scope-string scope))
	 (data1  (event-data->wire-data data)))
    (if (> (length data1) max-fragment-size)
	;; Split DATA1 into multiple fragment and make a notification
	;; for each fragment.
	(bind ((fragments     (fragment-data data1 max-fragment-size))
	       (num-fragments (length fragments)))
	  (iter (for fragment in    fragments)
		(for i        :from 0)
		(collect
		    (make-notification id1 scope1 "string" fragment
				       :meta-data      meta-data
				       :data-part      i
				       :num-data-parts num-fragments))))
	;; DATA1 fits into a single notification.
	(list
	 (make-notification id1 scope1 "string" data1
			    :meta-data meta-data)))))


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
       (rsb.converter:wire->domain
	:protocol-buffer data (pb::proto-type-name->lisp-type-symbol
				  designator)))
      (t
       (error "Unsupported wire-type/-schema (~S,~S)" mechanism designator)))))

(defun event-data->wire-data (data)
  "DOC"
  (typecase data
    (octet-vector data)
    (string       (sb-ext:string-to-octets data))
    (t            (rsb.converter:domain->wire
		   :protocol-buffer data 'octet-vector))))


;;; Utility functions
;;

(defun make-notification (id scope wire-schema data
			  &key
			  meta-data
			  (num-data-parts 1)
			  (data-part      0))
  "Make a `rsb.protocol:notification' instance with ID, SCOPE,
WIRE-SCHEMA, DATA and optionally META-DATA. When NUM-DATA-PARTS and
DATA-PART are not supplied, values that indicate a non-fragmented
notification are chosen."
  (bind ((attachment   (make-instance 'rsb.protocol::attachment
				      :length (length data)
				      :binary data))
	 (notification (make-instance 'rsb.protocol::notification
				      :id             id
				      :scope          scope
				      :wire-schema    wire-schema
				      :num-data-parts num-data-parts
				      :data-part      data-part
				      :data           attachment)))

    ;; Add META-DATA.
    (iter (for (key value) on meta-data :by #'cddr)
	  (vector-push-extend
	   (make-instance 'rsb.protocol::meta-info
			  :key   (prin1-to-string key)
			  :value (prin1-to-string value))
	   (rsb.protocol::notification-meta-infos notification)))

    ;; Return the complete notification instance.
    notification))

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
