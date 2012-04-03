;;; protocol.lisp --- Protocol of the transport module.
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

(defmethod connector-direction ((connector class))
  "Stop if we hit a class which is not a `connector-class'."
  (values))

(defmethod connector-wire-type ((connector standard-object))
  "Default behavior is to retrieve the wire-type from the class of
CONNECTOR."
  (connector-wire-type (class-of connector)))

(defmethod connector-wire-type ((class class))
  "Stop if we hit a class which is not a `connector-class'."
  (values))

(defmethod connector-schemas ((connector standard-object))
  "Default behavior is to retrieve the list of schemas from the class
of CONNECTOR."
  (connector-schemas (class-of connector)))

(defmethod connector-schemas ((class class))
  "Default behavior is to claim to support no schemas."
  nil)

(defmethod connector-options ((class class))
  "Default behavior is to claim to accept no options."
  nil)


;;; Message receiver protocol
;;

(defgeneric receive-message (connector block?)
  (:documentation
   "Receive and return one message via CONNECTOR. If BLOCK? is nil,
only return a message, if one is immediately available, otherwise
return nil. If BLOCK? is non-nil, wait for a message if no message is
immediately available. If something has been received, return two
values: the received message and a symbol designating the
wire-schema of the received data."))

(defgeneric message->event (connector message wire-schema)
  (:documentation
   "Convert MESSAGE with wire-schema WIRE-SCHEMA into an `event'
instance and return the event. If message cannot be converted into an
event, return nil instead. Signal a `decoding-error' if something goes
wrong."))


;;; Notification sender protocol
;;

(defgeneric send-notification (connector notification)
  (:documentation
   "Send NOTIFICATION via CONNECTOR."))

(defgeneric event->notification (connector event)
  (:documentation
   "Convert EVENT into a notification for sending via
CONNECTOR. Return the notification. If EVENT cannot be converted into
a notification, maybe return nil, depending on the error handling
policy. Maybe signal an `encoding-error' if something goes wrong."))


;;; Threaded receiver protocol
;;

(defgeneric start-receiver (connector)
  (:documentation
   "Ask CONNECTOR to start a receiver thread that runs
`receive-messages' until interrupted."))

(defgeneric stop-receiver (connector)
  (:documentation
   "Ask CONNECTOR to stop receiving messages."))

(defgeneric receive-messages (connector)
  (:documentation
   "CONNECTOR receives and processes messages until interrupted."))


;;; Transport metric exposing protocol
;;

(defgeneric connector-expose (connector)
  (:documentation
   "Return the list of transport metrics exposed by CONNECTOR."))

(defgeneric (setf connector-expose) (new-value connector)
  (:documentation
   "Install NEW-VALUE as the list of transport metrics exposed by
CONNECTOR."))

(defgeneric connector-expose? (connector metric)
  (:documentation
   "Return non-nil when CONNECTOR exposes METRIC."))

(defgeneric (setf connector-expose?) (new-value connector metric)
  (:documentation
   "If NEW-VALUE is non-nil, add METRIC to the list of transport
metrics exposed by CONNECTOR. Otherwise remove METRIC from the
list."))


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

(defun make-connector (name direction converters &rest args)
  "Create a connector instance for the direction designated by
DIRECTION of the kind the designated by NAME. Pass ARGS to the
constructed instance.
CONVERTERS is an alist of items of the form (WIRE-TYPE
. CONVERTER). If the requested connector does not require a converter,
CONVERTERS can be nil.
An error of type `connector-constructor-failed' is signaled if the
requested connector instance cannot be constructed. If the
construction fails due to the lack of a suitable converter, an error
of the subtype `no-suitable-converter' is signaled."
  (handler-bind
      (((and error (not no-suitable-converter))
	#'(lambda (condition)
	    (error 'connector-construction-failed
		   :name      name
		   :direction direction
		   :args      args
		   :cause     condition))))
    (let* ((class     (find-connector-class name direction))
	   (wire-type (connector-wire-type class))
	   (converter (unless (eq wire-type t)
			(or (getf args :converter)
			    (cdr (find wire-type converters
				       :key  #'car
				       :test #'subtypep)))))
	   (args      (remove-from-plist args :converter)))
      (cond
	;; The connector does not require a converter.
	((eq wire-type t)
	 (apply #'make-instance class args))
	;; The connector requires a converter and we found a suitable
	;; one.
	(converter
	 (apply #'make-instance class :converter converter args))
	;; The connector requires a converter, but we did not find a
	;; suitable one.
	(t
	 (error 'no-suitable-converter
		:name       name
		:direction  direction
		:args       args
		:wire-type  wire-type
		:candidates converters))))))

(defun make-connectors (specs direction &optional converters)
  "Create and return zero or more connector instances for the
direction designated by DIRECTION according to SPECS. Each element of
SPECS has to be of the form

  (NAME . ARGS)

where NAME and ARGS have to acceptable for calls to
`make-connector'. For CONVERTERS, see `make-connector'."
  ;; Check direction here in order to signal a appropriate type error
  ;; even if SPECS is nil.
  (check-type direction direction "either :IN-PUSH, :IN-PULL or :OUT")

  (iter (for (name . args) in specs)
	(collect (apply #'make-connector
			name direction converters args))))
