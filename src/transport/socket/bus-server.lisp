;;; bus-server.lisp --- A class that accepts connections from bus clients.
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


;;; Global map of bus servers
;;

(defvar *bus-servers* (make-hash-table :test #'equalp)
  "Map host names and ports to `bus-servers' instances.")

(defvar *bus-servers-lock* (bt:make-recursive-lock "Bus servers lock")
  "A lock that protects accesses to `*bus-servers*'.")

(defun ensure-bus-server (host port connector)
  "Return (creating it if necessary), a `bus-server' instance for the
endpoint designated by HOST and PORT and attach CONNECTOR to it.
Attaching CONNECTOR marks the `bus-server' instance as being in use
and protects it from being destroyed in a race condition situation."
  (log1 :trace "Trying to obtain bus server ~S:~D for ~A"
	host port connector)
  (let ((options (make-connection-options connector))
	(key     (cons host port)))
    (bt:with-recursive-lock-held (*bus-servers-lock*)
      (or (when-let ((candidate (gethash key *bus-servers*)))
	    (with-locked-bus (candidate)
	      (when (bus-connectors candidate)
		(check-connection-options (bus-options candidate) options)
		(notify connector candidate :attached)
		candidate)))
	  (let ((bus (make-instance 'bus-server
				    :host    host
				    :port    port
				    :options options)))
	    (notify connector bus :attached)
	    (setf (gethash key *bus-servers*) bus))))))


;;; `bus-server' class
;;

(defclass bus-server (bus
		      threaded-receiver-mixin)
  ((socket :reader   bus-socket
	   :writer   (setf %bus-socket)
	   :documentation
	   "Stores the listening socket which accepts client
connections."))
  (:documentation
   "An instance of this class provides access to a bus through a
listening socket to which bus clients connect. Each client connection
causes a `bus-connection' instance to be added to the list of
connections. These objects are removed when the connections are
closed."))

(defmethod shared-initialize :after ((instance   bus-server)
                                     (slot-names t)
                                     &key
				     host
				     port)
  ;; Setup the listening socket.
  (setf (%bus-socket instance)
	(usocket:socket-listen host port
			       :element-type '(unsigned-byte 8)))
  (log1 :info instance "Opened listen socket")

  (log1 :info instance "Starting acceptor thread")
  (start-receiver instance))

(defmethod notify ((bus     bus-server)
		   (subject (eql t))
		   (action  (eql :detached)))
  ;; First, stop the acceptor thread to prevent access to the socket.
  (log1 :info bus "Stopping acceptor thread")
  (unwind-protect
       (stop-receiver bus)

    ;; Close the listener socket to prevent new client connections.
    (log1 :info bus "Closing listen socket")
    (usocket:socket-close (bus-socket bus))

    ;; Close existing connections.
    (call-next-method)))


;;; Accepting clients
;;

(defmethod receive-messages ((bus bus-server))
  (let+ (((&accessors (server-socket bus-socket)
		      (connections   bus-connections)
		      (options       bus-options)) bus))
    ;; Main processing loop. Wait for activity on the server socket.
    ;; The loop is terminated by external interruption.
    ;;; TODO(jmoringe): we can leak the socket if we are interrupted here
    (iter (let ((client-socket (usocket:socket-accept
				server-socket
				:element-type '(unsigned-byte 8))))
	    ;; Since we create and add the new connection with the bus
	    ;; lock held, all events published on BUS after the
	    ;; handshake of the new connection completes are
	    ;; guaranteed to be delivered to the new connection. Also
	    ;; note that the server role of the handshake, sending 4
	    ;; bytes, usually does not involve blocking.
	    (with-locked-bus (bus)
	      (push (apply #'make-instance 'bus-connection
			   :socket    client-socket
			   :handshake :send
			   options)
		    connections))
	    (log1 :info bus "Accepted bus client ~/rsb.transport.socket::print-socket/"
		  client-socket)))))


;;;
;;

(defmethod print-object ((object bus-server) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(S ~D) (C ~D) ~:/rsb.transport.socket::print-socket/"
	    (length (bus-connections object))
	    (length (bus-connectors  object))
	    (bus-socket object))))
