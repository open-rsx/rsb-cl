;;; protocol.lisp ---
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


;;; Meta data protocol
;;

(defgeneric meta-data-count (object)
  (:documentation
   "Return the number of meta-data items stored in OBJECT."))

(defgeneric meta-data-keys (object)
  (:documentation
   "Return a list of the keys of meta-data items stored in OBJECT."))

(defgeneric meta-data-values (object)
  (:documentation
   "Return a list of the values of meta-data items stored in
OBJECT."))

(defgeneric meta-data-plist (object)
  (:documentation
   "Return a plist of the meta-data items stored in OBJECT."))

(defgeneric meta-data-alist (object)
  (:documentation
   "Return an alist of the meta-data items stored in OBJECT."))

(defgeneric meta-data (object key)
  (:documentation
   "Return the meta-data item of OBJECT identified by KEY."))

(defgeneric (setf meta-data) (new-value object key)
  (:documentation
   "Associate NEW-VALUE to OBJECT as the meta-data item identified by
KEY."))


;;; Common protocol for receiving participants
;;

(defgeneric receiver-filters (receiver)
  (:documentation
   "Return the list of filters associated to the receiving participant
RECEIVER."))

(defgeneric (setf receiver-filters) (new-value receiver)
  (:documentation
   "Set the list of filters associated to the receiving participant
RECEIVER to NEW-VALUE."))


;;; Listener protocol
;;

(defgeneric make-listener (scope
			   &key
			   transports)
  (:documentation
   "Listen to events on the channel designated by SCOPE.
If successful return a `listener' instance. Otherwise an error of type
`listener-creation-failed' is signaled."))

;; TODO unlisten, also use detach?


;;; Reader protocol
;;

(defgeneric make-reader (scope
			 &key
			 transports)
  (:documentation
   "Receive events on the channel designated by SCOPE.
if successful, return a `reader' instance. Otherwise an error of type
`reader-creation-failed' is signaled. The resulting `reader' instance
can be used to receive data in \"pull\" manner using the `receive'
function."))

(defgeneric receive (reader
		     &key
		     block?)
  (:documentation
   "Receive data from the channel in which RECEIVER is
participating. When data is received, it is returned in form of an
`event' instance. If BLOCK? is non-nil, wait for data to become
available if there is none. If BLOCK/ is nil and no data is available,
nil is returned."))


;;; Informer protocol
;;

(defgeneric make-informer (scope type
			   &key
			   transports)
  (:documentation
   "Start publishing data of type TYPE on the channel designated by
SCOPE.
If successful, return an `informer' instance. Otherwise an error of
type `informer-creation-failed' is signaled."))

(defgeneric send (informer data)
  (:documentation
   "Send DATA to participants of the channel in which INFORMER
participates."))
