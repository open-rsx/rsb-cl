;;; connector.lisp ---
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

(defclass connector (rsb.transport:connector)
  ((connection :type     connection
	       :reader   connector-connection
	       :documentation
	       ""))
  (:default-initargs
   :schema :spread
   :host   (load-time-value (hostname) t))
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
	 ((:accessors-r/o (uri rsb.transport::connector-uri)) instance))
    (when hostname
      (setf (puri:uri-host uri) hostname))
    (setf (puri:uri-port uri) port)

    (setf (slot-value instance 'connection)
	  (or connection
	      (make-instance 'connection
			     :name name)))))

(defmethod notify ((connector connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 (group-name (scope->group scope)))
    (ref-group connection group-name))
  :implemented)

(defmethod notify ((connector connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (bind (((:accessors-r/o (connection connector-connection)) connector)
	 (group-name (scope->group scope)))
    (unref-group connection group-name))
  :implemented)
