;;; connection.lisp ---
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

;; TODO handlers
(defclass connection (rsb.ep:broadcast-processor)
  ((connection :initarg  :connection
	       :type     spread:connection
	       :documentation
	       "The underlying spread connection used by the connector
instance.")
   (groups     :initarg  :groups
	       :type     hash-table
	       :initform (make-hash-table :test #'equal)
	       :documentation
	       ""))
  (:documentation
   "DOC"))

(defmethod initialize-instance :after ((instance connection)
				       &key
				       name)
  (setf (slot-value instance 'connection) (spread:connect name)))

(defmethod ref-group ((connection connection) (group string))
  (when (= (incf (gethash group (slot-value connection 'groups) 0)) 1)
    (log1 :info "~A: joining group ~S." connection group)
    (spread:join (slot-value connection 'connection) group)))

(defmethod unref-group :around ((connection connection) (group string))
  (if (gethash group (slot-value connection 'groups))
      (call-next-method)
      (log1 :warn "~A: Was asked to unreference unknown group ~S." connection group))) ;; TODO use context for connection instance?

(defmethod unref-group ((connection connection) (group string))
  (when (zerop (decf (gethash group (slot-value connection 'groups) 0)))
    (log1 :info "~A: leaving group ~S." connection group)
    (remhash group (slot-value connection 'groups))
    (spread:leave (slot-value connection 'connection) group)))

(defmethod receive-message ((connection connection)
			    &key
			    (block? t))
  "DOC"
  (spread:receive (slot-value connection 'connection) :block? block?))

(defmethod send-message ((connection   connection)
			   (destination list)
			   (payload     simple-array))
  "DOC"
  (check-type payload spread:octet-vector "an octet-vector") ;; TODO rsb:octet-vector?

  (log1 :info "~A: message [me -> ~S] ~S." connection destination (subseq payload 0 (min 256 (length payload))))

  (spread:send (slot-value connection 'connection) destination payload))

(defmethod print-object ((object connection) stream)
  (with-slots (connection groups) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A (~D)"
	      (spread:connection-name connection)
	      (hash-table-count groups)))))
