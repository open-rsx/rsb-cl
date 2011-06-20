;;; connector.lisp --- Superclass for spread connectors.
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

(defclass connector (rsb.transport:connector
		     conversion-mixin)
  ((connection :type     connection
	       :reader   connector-connection
	       :documentation
	       ""))
  (:metaclass connector-class)
  (:default-initargs
   :schema :spread
   :host   (load-time-value (hostname) t))
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
    :default spread:*default-port*
    :description
    "The port number of the spread daemon. Mutually exclusive with NAME.")
   (:tcpnodelay boolean
    :default t
    :description
    "Should the TCP_NODELAY option be set on the socket used for Spread communication? Note: currently ignored by Lisp implementation."))
  (:documentation
   "This class serves as a superclass for spread in and out
connectors."))

(defmethod shared-initialize :after ((instance connector) (slot-names t)
				     &key
				     host
				     port
				     name
				     connection)
  (bind (((:values hostname port) (if (and host port)
				      (values host port)
				      (parse-spread-name name)))
	 (name (or (format nil "~D@~A" port hostname)))
	 ((:accessors-r/o (uri connector-url)) instance))
    (when hostname
      (setf (puri:uri-host uri) hostname))
    (setf (puri:uri-port uri) port)

    (setf (slot-value instance 'connection)
	  (or connection
	      (make-instance 'connection
			     :name name)))))
