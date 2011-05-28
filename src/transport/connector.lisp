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


;;; `connector-class' metaclass
;;

(defclass connector-class (standard-class)
  ((direction :type     direction
	      :reader   connector-direction
	      :documentation
	      "Stores the direction of instances of the connector
class.")
   (wire-type :type     (or symbol list)
	      :documentation
	      "Stores the wire-type of instance of the connector
class.")
   (schemas   :initarg  :schemas
	      :type     list
	      :initform nil
	      :documentation
	      "Stores a list of schemas provided by the connector
class.")
   (options   :initarg  :options
	      :type     list
	      :initform nil
	      :documentation
	      ""))
  (:documentation
   "This metaclass can be used as the class of connector classes in
order to provide storage and retrieval (via methods on
`connector-direction', `connector-wire-type', `connector-schemas' and
`connector-options') for connector direction, wire-type, schemas and
options."))

(defmethod shared-initialize :after ((instance   connector-class)
				     (slot-names t)
				     &key
				     wire-type
				     direction)
  (when wire-type
    (setf (slot-value instance 'wire-type) (first wire-type)))
  (when direction
    (setf (slot-value instance 'direction) (first direction))))

(defmethod connector-wire-type ((class class))
  (values))

(defmethod connector-wire-type ((class connector-class))
  "Use wire-type stored in CLASS or retrieve from superclasses if
necessary. "
  (if (slot-boundp class 'wire-type)
      (slot-value class 'wire-type)
      (some #'connector-wire-type
	    (closer-mop:class-direct-superclasses class))))

(defmethod connector-schemas ((class connector-class))
  "Retrieve supported schemas from CLASS and its transitive
superclasses."
  (apply #'append (slot-value class 'schemas)
	 (map 'list #'connector-schemas
	      (closer-mop:class-direct-superclasses class))))

(defmethod connector-options ((class connector-class))
  "Retrieve options from CLASS and its transitive superclasses.
Option definitions in subclasses take precedence over definitions in
superclasses."
  (let* ((my-options (slot-value class 'options))
	 (names      (map 'nil #'first my-options)))
    (apply #'append
	   my-options
	   (remove-if (lambda (option) (member (first option) names))
		      (map 'list #'connector-options
			   (closer-mop:class-direct-superclasses class))))))

(defmethod closer-mop:validate-superclass ((class      connector-class)
					   (superclass standard-class))
  t)


;;; `connector' class
;;

(defclass connector (uri-mixin)
  ((rsb::uri :reader   connector-url))
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
  (puri:merge-uris uri (connector-url connector)))

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
    (format stream "~A ~A"
	    (connector-direction object)
	    (connector-relative-url object "/"))))
