;;; bus.lisp --- Superclass for bus provider classes.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:in-package :rsb.transport.socket)

(defmacro with-locked-bus ((bus
			    &rest args
			    &key
			    connections?
			    connectors?)
			   &body body)
  "Execute BODY with BUS' lock held."
  (check-type connections? boolean)
  (check-type connectors?  boolean)

  (flet ((maybe-with-lock (which requested? body)
	   (if requested?
	       `((bt:with-recursive-lock-held ((,which ,bus))
		   ,@body))
	       body)))
    `(progn
       ,@(maybe-with-lock
	  'bus-connections-lock (or (not args) connections?)
	  (maybe-with-lock 'bus-connectors-lock (or (not args) connectors?)
	   body)))))

(defclass bus (broadcast-processor)
  ((connections      :type     list
		     :accessor bus-connections
		     :initform nil
		     :documentation
		     "Stores a list of connections to other processes
using the bus.")
   (connections-lock :reader   bus-connections-lock
		     :initform (bt:make-recursive-lock "Bus Connections Lock")
		     :documentation
		     "Stores a lock that can be used to protect the
connection list of the bus from concurrent modification.")
   (connectors       :type     list
		     :accessor bus-connectors
		     :initform nil
		     :documentation
		     "Stores a list of local connectors connected to
the bus.")
   (connectors-lock  :reader   bus-connectors-lock
		     :initform (bt:make-recursive-lock "Bus Connectors Lock")
		     :documentation
		     "Stores a lock that can be used to protect the
connector list of the bus from concurrent modification.")
   (options          :initarg  :options
		     :type     list
		     :accessor bus-options
		     :initform nil
		     :documentation
		     "Stores a plist of connection options which
should be used by connections associated to the bus instance.")
   (proxy            :type     function
		     :accessor %bus-proxy
		     :documentation
		     "Stores a functions that is used as a handler for
`bus-connection' instances."))
  (:default-initargs
   :host (missing-required-initarg 'in-connector :host)
   :port (missing-required-initarg 'in-connector :port))
  (:documentation
   "This class is intended to be used as a superclass of client and
server providers of bus access. It manages a list of connections to
remote processes using the bus and a list of local connectors
connected to the bus."))

(defmethod shared-initialize :after ((instance   bus)
                                     (slot-names t)
                                     &key)
  (setf (%bus-proxy instance)
	#'(lambda (connection data)
	    (handle instance (cons connection data)))))


;;; State management
;;

(defmethod (setf bus-connections) :around ((new-value   list)
					   (bus         bus))
  (let+ (((&accessors-r/o (old-value bus-connections)
			  (proxy     %bus-proxy)) bus))
    (declare (type function proxy))
    (prog1
	(call-next-method)
      (let ((added   (set-difference new-value old-value))
	    (removed (set-difference old-value new-value)))
	(log1 :info bus "Added   connections ~{~S~^, ~}" added)
	(log1 :info bus "Removed connections ~{~S~^, ~}" removed)

	;; Install our handler and error policy in added connections.
	(iter (for connection in added)
	      ;; Add ourselves as handlers to the added connection.
	      (push (curry proxy connection) (handlers connection))

	      ;; Install an error policy that removes the connection.
	      (log1 :info bus "Installing error policy for connection ~A"
		    connection)
	      (setf (processor-error-policy connection)
		    #'(lambda (condition)
			(declare (ignore condition))
			(log1 :info bus "Removing connection ~A after error policy" connection)
			(with-locked-bus (bus :connections? t)
			  (removef (bus-connections bus) connection))))

	      ;; Start the connection.
	      (log1 :info bus "Starting connection ~A" connection)
	      (start-receiver connection))

	;; Close removed connections.
	(iter (for connection in removed)
	      ;; Prevent the error handling from being executed
	      ;; concurrently/recursively.
	      (log1 :info bus "Maybe closing connection ~A after remove" connection)
	      (handler-case (disconnect connection :handshake :send)
		(error (condition)
		  (log1 :warn bus "Error closing connection ~A after remove: ~A"
			connection condition))))))))

(defmethod (setf bus-connectors) :around ((new-value list)
					  (bus       bus))
  (let ((old-value (bus-connectors bus)))
    (prog1
	(call-next-method)
      (cond
	((and old-value (not new-value))
	 (log1 :info bus "No more connectors")
	 (notify bus t :detached))
	((and (not old-value) new-value)
	 (log1 :info bus "First connector")
	 (notify bus t :attached))))))

(defmethod notify ((connector rsb.transport:connector)
		   (bus       bus)
		   (action    (eql :attached)))
  (log1 :info bus "Attaching connector ~A to bus provider ~A"
	connector bus)
  (with-locked-bus (bus)
    (push connector (bus-connectors bus))))

(defmethod notify ((connector rsb.transport:connector)
		   (bus       bus)
		   (action    (eql :detached)))
  (log1 :info bus "Detaching connector ~A from bus provider ~A"
	connector bus)
  (with-locked-bus (bus)
    (removef (bus-connectors bus) connector)))

(defmethod notify ((bus     bus)
		   (subject (eql t))
		   (action  (eql :detached)))
  "Remove connections when all connectors detach."
  (setf (bus-connections bus) nil))


;;; Sending and receiving
;;

(defmethod dispatch ((bus  bus)
		     (data notification))
  "Dispatch DATA to interested connectors."
  (let* ((scope    (make-scope
		    (bytes->string (notification-scope data))))
	 (handlers (with-locked-bus (bus :connectors? t)
		     (copy-list (handlers bus))))
	 (sinks    (remove-if-not
		    #'(lambda (connector)
			(and (member (connector-direction connector)
				     '(:in-push :in-pull))
			     (sub-scope? scope (connector-scope connector))))
		    handlers)))
    (handle sinks data)))

(defmethod handle ((bus          bus)
		   (notification notification))
  "This method is used for outgoing notifications."
  ;; Send to remote peer(s).
  (with-locked-bus (bus :connections? t)
    (map nil (rcurry #'handle notification) (bus-connections bus)))

  ;; Dispatch to our own connectors.
  (dispatch bus notification))

(defmethod handle ((bus  bus)
		   (data cons))
  "This method is used for incoming notifications."
  (let+ (((received-via . notification) data))
    ;; Dispatched to all connections except the one from which we
    ;; received the notification.
    (with-locked-bus (bus :connections? t)
      (iter (for connection in (bus-connections bus))
	    (unless (eq received-via connection)
	      ;; We can ignore errors here since we installed an error
	      ;; policy in CONNECTION that removes CONNECTION.
	      (ignore-errors
		(handle connection notification)))))

    ;; Dispatch to our own connectors.
    (dispatch bus notification)))


;;;
;;

(defmethod print-object ((object bus) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(S ~D) (C ~D)"
	    (length (bus-connections object))
	    (length (bus-connectors  object)))))
