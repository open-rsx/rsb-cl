;;; connector-class.lisp --- Metaclass for connector classes.
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

(cl:in-package :rsb.transport)

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
  (let+ (((name type &rest nil) option))
    (or (unless (eq type '&slot)
	  option)

	(when-let ((slot (find name (closer-mop:class-slots class)
			       :test #'member
			       :key  #'closer-mop:slot-definition-initargs)))
	  `(,name
	    ,(closer-mop:slot-definition-type slot)
	    ,@(when-let ((initform (closer-mop:slot-definition-initform slot)))
	        `(:default ,initform))
	    ,@(when-let ((description (documentation slot t)))
	        `(:description ,description))))

	(error "~@<~S specified for option ~S, but no slot with ~
initarg ~:*~S in class ~S.~@:>"
	       '&slot name class))))
