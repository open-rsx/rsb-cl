;;; connector.lisp --- Superclass for socket-based connectors.
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

(cl:in-package :rsb.transport.socket)

(defclass connector (rsb.transport:connector
		     conversion-mixin)
  ((bus      :accessor connector-bus
	     :documentation
	     "Stores the bus object representing the socked-based
bus to which the connector provides access.")
   ;; Option slots
   (host     :initarg  :host
	     :type     string
	     :reader   connector-host
	     :documentation
	     "The name of the host on which the server is listener in case of clients and the bind address in case of the server.")
   (port     :initarg  :port
	     :type     (unsigned-byte 16)
	     :reader   connector-port
	     :documentation
	     "The port on which the server is listening in case of clients and the port on which connections should be accepted in case of the server.")
   (server?  :initarg  :server
	     :initarg  :server?
	     :type     t ;;; TODO(jmoringe): was boolean; we can change this back when we get proper configuration
	     :reader   connector-server?
	     :initform nil
	     :documentation
	     "Controls whether the connector takes the server or client role for the bus.")
   (nodelay? :initarg  :nodelay
	     :initarg  :nodelay?
	     :type     t
	     :reader   connector-nodelay?
	     :initform nil
	     :documentation
	     "Controls whether decreased troughput should be traded for reduced latency by the connector. For TCP connections this means the TCPNODELAY option should be set on the socket implementing the bus connection."))
  (:default-initargs
   :host (missing-required-initarg 'in-connector :host)
   :port (missing-required-initarg 'in-connector :port))
  (:metaclass connector-class)
  (:wire-type octet-vector)
  (:schemas   :socket)
  (:options
   (:host     &slot)
   (:port     &slot)
   (:server?  &slot)
   (:nodelay? &slot))
  (:documentation
   "This class serves as a superclass for connector classes that
employ socked-based bus access."))

(defmethod notify ((connector connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (let+ (((&accessors (host    connector-host)
		      (port    connector-port)
		      (server? connector-server?)
		      (bus     connector-bus)) connector))
    ;; Depending on whether connecting to the socket-based bus as a
    ;; client or server has been requested, request a suitable bus
    ;; access provider.
    ;;; TODO(jmoringe, 2011-12-14): temp solution until config system works properly
    (setf bus (%get-bus host port
			(etypecase server?
			  ((member t nil :auto) )
			  (string
			   (cond
			     ((string= server? "0")    nil)
			     ((string= server? "1")    t)
			     ((string= server? "auto") :auto))))
			connector))

    ;; Notify the bus access provider of the added connector.
    ;; ensure-bus-* already attached CONNECTOR.
    ;; (notify connector bus :attached)
    (when (next-method-p)
      (call-next-method))))

(defmethod notify ((connector connector)
		   (scope     scope)
		   (action    (eql :detached)))
  ;; Notify the bus access provider of the removed connector.
  (notify connector (connector-bus connector) :detached)

  (when (next-method-p)
    (call-next-method)))


;;; Utility functions
;;

(defun %get-bus (host port server? connector)
  "Depending on SERVER?, find a bus provider for HOST and PORT and add
CONNECTOR to it. If SERVER? is :AUTO, first try to create a server
provider and try to fall back to a client provider if that fails."
  (ecase server?
    ((t)
     (restart-case
	 (ensure-bus-server host port connector)
       (retry-as-client ()
	 :report (lambda (stream)
		   (format stream "~@<Retry connecting to the bus at ~
~A:~D as client.~@:>"
			   host port))
	 (%get-bus host port nil connector))))
    ((nil)
     (restart-case
	 (ensure-bus-client host port connector)
       (retry-as-server ()
	 :report (lambda (stream)
		   (format stream "~@<Try to create a server for the ~
bus at ~A:~D.~@:>"
			   host port))
	 (%get-bus host port t connector))))
    ;; Try to create a server bus provider and fall back to connecting
    ;; via a client bus provider if the server bus provider could not
    ;; be created.
    (:auto
     (handler-case
	 (ensure-bus-server host port connector)
       ((or usocket:address-in-use-error
	    usocket:address-not-available-error
	    #+sbcl sb-bsd-sockets:socket-error) (server-condition)
	 (handler-bind
	     ((error (lambda (client-condition)
		       (error 'socket-bus-auto-connection-error
			      :format-control "Failed to get ~
socket-based bus as server:~&~@<> ~@;~A~@>~&Failed to get socket-based ~
bus as client:~&~@<> ~@;~A~@>"
			      :format-arguments (list server-condition
						      client-condition)))))
	   (ensure-bus-client host port connector)))))))