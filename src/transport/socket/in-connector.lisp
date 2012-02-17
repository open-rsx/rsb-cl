;;; in-connector.lisp ---
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

(cl:in-package :rsb.transport.socket)

(defclass in-connector (connector
			restart-message-receiver-mixin
			broadcast-processor
			expose-transport-metrics-mixin)
  ((scope :type     scope
	  :accessor connector-scope
	  :documentation
	  "Stores the scope to which the connector is attached."))
  (:metaclass connector-class)
  (:documentation
   "Superclass for in-direction socket connectors. Instances of this
class observe a bus (which owns the actual socket) when attached and
queue received events for delivery."))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (setf (connector-scope connector) scope)
  (call-next-method)
  (push connector (handlers (connector-bus connector))))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (removef (handlers (connector-bus connector)) connector)
  (call-next-method))

(defmethod message->event ((connector    in-connector)
			   (notification notification)
			   (wire-schema  t))
  (let+ (((&accessors-r/o (converter connector-converter)) connector)
	 (expose-wire-schema?  (connector-expose?
				connector :rsb.transport.wire-schema))
	 (expose-payload-size? (connector-expose?
				connector :rsb.transport.payload-size)))

    ;; If message could be unpacked into a `notification' instance,
    ;; try to convert it, and especially its payload, into an `event'
    ;; instance and an event payload. There are three possible
    ;; outcomes:
    ;; 1. The notification (maybe in conjunction with previously
    ;;    received notifications) forms a complete event
    ;;    a) The payload conversion succeeds
    ;;       In this case, an `event' instance is returned
    ;;    b) The payload conversion fails
    ;;       In this case, an error is signaled
    ;; 2. The notification does not form a complete event
    ;;    In this case, nil is returned.
    (handler-bind
	((error #'(lambda (condition)
		    (error 'decoding-error
			   :encoded          (list notification) ;;; TODO(jmoringe): hack
			   :format-control   "~@<After unpacking, the ~
notification~_~A~_could not be converted into an event.~:@>"
			   :format-arguments `(,(with-output-to-string (stream)
								       (describe notification stream)))
			   :cause            condition))))
      (notification->event converter notification
			   :expose-wire-schema?  expose-wire-schema?
			   :expose-payload-size? expose-payload-size?))))
