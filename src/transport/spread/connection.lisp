;;; connection.lisp --- Spread connections with membership management.
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

(cl:in-package :rsb.transport.spread)


;;; Spread connection protocol
;;

(defgeneric ref-group (connection group)
  (:documentation
   "Increase the reference count of GROUP, causing CONNECTION to join
GROUP in case of a 0 -> 1 transition."))

(defgeneric unref-group (connection group)
  (:documentation
   "Decrease the reference count of GROUP, causing CONNECTION to leave
GROUP in case of a 1 -> 0 transition."))

(defgeneric send-message (connection destination payload)
  (:documentation
   "Send the PAYLOAD to DESTINATION via CONNECTION in a Spread
message."))


;;; `connection' class
;;

(defclass connection ()
  ((connection :initarg  :connection
	       :type     spread:connection
	       :documentation
	       "The underlying spread connection used by the connector
instance.")
   (groups     :type     hash-table
	       :initform (make-hash-table :test #'equal)
	       :documentation
	       "A mapping of group names to reference counts. The
connection instance is a member of every group that has a positive
reference count."))
  (:documentation
   "Instances of this class represent connections to Spread
communication system and maintain a list of the Spread multicast
groups in which they are members."))

(defmethod initialize-instance :before ((instance connection)
					&key
					connection
					name)
  (unless (or name connection)
    (missing-required-initarg 'connector :either-name-or-connection)))

(defmethod shared-initialize :before ((instance   connection)
				      (slot-names t)
				      &key
				      connection
				      name)
  (when (and name connection)
    (error 'incompatible-initargs
	   :class      'connection
	   :parameters '(:name :connection)
	   :values     (list name connection))))

(defmethod shared-initialize :after ((instance   connection)
                                     (slot-names t)
                                     &key
				     name)
  (when name
    (setf (slot-value instance 'connection) (spread:connect name))))

(defmethod ref-group ((connection connection) (group string))
  (when (= (incf (gethash group (slot-value connection 'groups) 0)) 1)
    (log1 :info connection "Joining group ~S" group)
    (spread:join (slot-value connection 'connection) group)))

(defmethod unref-group :around ((connection connection) (group string))
  (if (gethash group (slot-value connection 'groups))
      (call-next-method)
      (log1 :warn connection "Was asked to unreference unknown group ~S" group)))

(defmethod unref-group ((connection connection) (group string))
  (when (zerop (decf (gethash group (slot-value connection 'groups) 0)))
    (log1 :info connection "leaving group ~S" group)
    (remhash group (slot-value connection 'groups))
    (spread:leave (slot-value connection 'connection) group)))

(macrolet
    ((with-spread-condition-translation (&body body)
       `(handler-bind
	    ((spread:spread-error
	       #'(lambda (condition)
		   (when (member (spread:spread-error-code condition)
				 '(:net-error-on-session :connection-closed))
		     (error 'connection-unexpectedly-closed
			    :connection connection
			    :cause      condition)))))
	  ,@body)))

  (defmethod receive-message ((connection connection) (block? t))
    (with-spread-condition-translation
	(spread:receive (slot-value connection 'connection) :block? block?)))

  (defmethod send-message ((connection  connection)
			   (destination list)
			   (payload     simple-array))
    (check-type payload octet-vector "an octet-vector")

    (with-spread-condition-translation
	(spread:send-bytes
	 (slot-value connection 'connection) destination payload))))

(defmethod print-object ((object connection) stream)
  (with-slots (connection groups) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A (~D)"
	      (spread:connection-name connection)
	      (hash-table-count groups)))))
