;;; connector-class.lisp --- Metaclass for connector classes.
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
   (options   :type     list
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
				     direction
				     options)
  (when wire-type
    (setf (slot-value instance 'wire-type) (first wire-type)))
  (when direction
    (setf (slot-value instance 'direction) (first direction)))

  ;; Process options.
  (closer-mop:finalize-inheritance instance)
  (setf (slot-value instance 'options)
	(map 'list (curry #'%maybe-expand-option instance) options)))

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


;;; Utility functions
;;

(defun %maybe-expand-option (class option)
  "Potentially expand the options description OPTION using information
from CLASS."
  (bind (((name type &rest _) option))
    (if (eq type '&slot)
	(let* ((slot        (find name (closer-mop:class-slots class)
				  :key (compose #'make-keyword
						#'closer-mop:slot-definition-name)))
	       (initform    (closer-mop:slot-definition-initform slot))
	       (description (documentation slot t)))
	  (unless slot
	    (error "~@<No slot named ~S in class ~S.~@:>" name class))
	  `(,name
	    ,(closer-mop:slot-definition-type slot)
	    ,@(when initform
	        `(:default ,initform))
	    ,@(when description
		`(:description ,description))))
	option)))
