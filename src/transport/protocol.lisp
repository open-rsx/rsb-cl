;;; protocol.lisp ---
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


;;; Connector protocol
;;

(defgeneric connector-url (connector)
  (:documentation
   "Return a base URL that can be used to locate resources via
CONNECTOR."))

(defgeneric connector-relative-url (connector thing)
  (:documentation
   "Return a complete URL suitable for locating the resource THING via
 CONNECTOR."))


;;; Connector introspection protocol
;;

(defgeneric connector-direction (connector)
  (:documentation
   "Return the communication direction of CONNECTOR.
Connector can be a connector class or a connector instance."))

(defgeneric connector-wire-type (connector)
  (:documentation
   "Return the wire-type of CONNECTOR.
Connector can be a connector class or a connector instance."))

(defgeneric connector-schemas (class)
  (:documentation
   "Return a list of the (URI-)schemas supported by the connector
class CLASS."))

(defgeneric connector-options (class)
  (:documentation
   "Return a description of the options accepted by the connector
class CLASS. The returned description is a list of items of the
form (NAME TYPE &optional DOCUMENTATION) where name is a keyword which
names the option and TYPE is the type of acceptable values of the
option."))


;;; Default behavior
;;

(defmethod connector-direction ((connector standard-object))
  "Default behavior is to retrieve the direction from the class of
CONNECTOR."
  (connector-direction (class-of connector)))

(defmethod connector-wire-type ((connector standard-object))
  "Default behavior is to retrieve the wire-type from the class of
CONNECTOR."
  (connector-wire-type (class-of connector)))

(defmethod connector-schemas ((class class))
  "Default behavior is to claim to support no schemas."
  nil)

(defmethod connector-options ((class class))
  "Default behavior is to claim to accept no options."
  nil)


;;; Transport implementations
;;

(dynamic-classes:define-findable-class-family transport
    "Transports are implemented by input and output connector
classes. These are designated by names of the
form :TRANSPORT-in-push, :TRANSPORT-in-pull and :TRANSPORT-out
respectively.")

(defun find-connector-class (name direction)
  "Return the connector class designated by NAME for the direction
designated by DIRECTION. DIRECTION has to be one of :in-push, :in-pull
and :out. An error of type `no-such-transport-class' is signaled if
the requested class cannot be found."
  (check-type name      keyword   "a keyword")
  (check-type direction direction "either :IN-PUSH, :IN-PULL or :OUT")

  (let ((name1 (let ((*package* (find-package :keyword)))
		 (symbolicate name "-" direction))))
    (find-transport-class name1)))

(defun make-connector (name direction &rest args)
  "Create a connector instance for the direction designated by
DIRECTION of the kind the designated by NAME. Pass ARGS to the
constructed instance."
  (apply #'make-instance (find-connector-class name direction) args))

(defun make-connectors (specs direction)
  "Create zero or more connector instances for the direction
designated by DIRECTION according to SPECS. Each element of SPECS has
to be of the form (NAME . ARGS) where NAME and ARGS have to acceptable
for calls to `make-connector'."
  ;; Check direction here in order to signal a appropriate type error
  ;; even if SPECS is nil.
  (check-type direction direction "either :IN-PUSH, :IN-PULL or :OUT")

  (iter (for (name . args) in specs)
	(collect (apply #'make-connector name direction args))))
