;;; bus-client.lisp --- A bus provider that used a client socket.
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

(in-package :rsb.transport.socket)


;;; Global map of bus client connections
;;

(defvar *bus-clients* (make-hash-table :test #'equalp)
  "Map host names and ports to `bus-client' instances.")

(defvar *bus-clients-lock* (bt:make-lock "Bus clients lock")
  "A lock that protects accesses to `*bus-clients*'.")

(defun ensure-bus-client (host port connector)
  "Return (creating it if necessary), a `bus-client' instance for the
endpoint designated by HOST and PORT and attach CONNECTOR to it.
Attaching CONNECTOR marks the `bus-client' instance as being in use
and protects it from being destroyed in a race condition situation."
  (let ((options (make-connection-options connector))
	(key     (cons host port)))
    (bt:with-lock-held (*bus-clients-lock*)
      (or (when-let ((candidate (gethash key *bus-clients*)))
	    (with-locked-bus (candidate)
	      (when (bus-connectors candidate)
		(check-connection-options (bus-options candidate) options)
		(notify connector candidate :attached)
		candidate)))
	  (let ((bus (make-instance 'bus-client
				    :host    host
				    :port    port
				    :options options)))
	    (notify connector bus :attached)
	    (setf (gethash key *bus-clients*) bus))))))


;;; `bus-client' class
;;

(defclass bus-client (bus)
  ()
  (:documentation
   "Instances of this class provide access to a bus by means of a
client socket."))

(defmethod shared-initialize :after ((instance   bus-client)
                                     (slot-names t)
                                     &key
				     host
				     port
				     options)
  ;; Add a single connection to INSTANCE. The returned connection is
  ;; guaranteed to have completing the handshake and thus receives all
  ;; events published on the bus afterward.
  (setf (bus-connections instance)
	(list (apply #'make-instance 'bus-connection
		     :host      host
		     :port      port
		     :handshake :receive
		     options))))
