;;; connector.lisp --- Superclass for spread connectors.
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

(cl:in-package :rsb.transport.spread)

(defclass connector (rsb.transport:connector
		     conversion-mixin)
  ((connection :initarg  :connection
	       :type     connection
	       :reader   connector-connection
	       :documentation
	       ""))
  (:metaclass connector-class)
  (:default-initargs
   :schema :spread)
  (:wire-type octet-vector)
  (:schemas   :spread)
  (:options
   (:name string
    :description
    "The name of the spread daemon. Has to be either of the form PORT@HOSTNAME or just PORT. Mutually exclusive with HOST and PORT.")
   (:host string
    :description
    "The hostname of the spread daemon. Mutually exclusive with NAME.")
   (:port (integer 0 65534)
    :default network.spread:*default-port*
    :description
    "The port number of the spread daemon. Mutually exclusive with NAME.")
   (:tcpnodelay boolean
    :default t
    :description
    "Should the TCP_NODELAY option be set on the socket used for Spread communication? Note: currently ignored by Lisp implementation."))
  (:documentation
   "This class serves as a superclass for spread in and out
connectors."))

(defmethod initialize-instance :before ((instance connector)
					&key
					connection
					name
					port)
  "Make sure that at least one of CONNECTION, NAME and PORT is
supplied."
  (unless (or connection name port)
    (missing-required-initarg
     'connector :either-connection-or-name-or-port)))

(defmethod shared-initialize :after ((instance connector) (slot-names t)
				     &key
				     connection
				     name
				     host
				     port)
  (let+ (((&values host port)
	  (cond
	    (name            (network.spread:parse-daemon-name name))
	    ((and host port) (values host port))
	    (port            (values nil  port))))
	 (name (format nil "~D~@[@~A~]" port host))
	 ((&accessors-r/o (uri connector-url)) instance))
    (when host
      (setf (puri:uri-host uri) host))
    (setf (puri:uri-port uri) port)

    ;; Unless a connection has been supplied, connect to the spread
    ;; daemon designated by NAME.
    (unless connection
      (setf (slot-value instance 'connection)
	    (make-instance 'connection :name name)))))
