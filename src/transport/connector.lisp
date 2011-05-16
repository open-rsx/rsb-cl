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

(in-package :rsb.transport)

(defclass connector (uri-mixin)
  ((rsb::uri :reader connector-uri)) ;; TODO base uri
  (:documentation
   "A connector implements access to the bus by means of a particular
mechanism. One example is a connector that makes use of the Spread
group communication framework."))

(defmethod shared-initialize :after ((instance connector) (slot-names t)
				     &key
				     schema
				     host
				     port
				     &allow-other-keys)
  (setf (slot-value instance 'rsb::uri)
	(make-instance 'puri:uri
		       :scheme schema
		       :host   host
		       :port   port)))

(defmethod connector-relative-url ((connector connector)
				   (uri       puri:uri))
  (puri:merge-uris uri (connector-uri connector)))

(defmethod connector-relative-url ((connector connector)
				   (scope     rsb::scope)) ;; TODO can we remove this? scope has relative-url
  "DOC"
  (connector-relative-url
   connector
   (make-instance 'puri:uri
				  :path        (rsb::scope-string scope)
				  ;;:parsed-path (rsb::scope-components scope)
				  )
   ))

(defmethod connector-relative-url ((connector connector)
				   (thing     string))
  (connector-relative-url connector (make-scope thing)))

(defmethod connector-relative-url ((connector connector)
				   (thing     t))
  (connector-relative-url connector (rsb::relative-url thing)))

(defmethod print-object ((object connector) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~A" (connector-relative-url object "/")))) ;; TODO direction
