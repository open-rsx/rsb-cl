;;; in-connector.lisp --- Superclass for in-direction connector classes.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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


;;; `in-connector' class
;;

(defclass in-connector (connector
			timestamping-receiver-mixin
			restart-message-receiver-mixin
			broadcast-processor
			assembly-mixin
			expose-transport-metrics-mixin)
  ()
  (:metaclass connector-class)
  (:documentation
   "This class is intended to be used as a superclass of in-direction
connector classes for Spread."))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :attached)))
  (ref-group (connector-connection connector) (scope->group scope)))

(defmethod notify ((connector in-connector)
		   (scope     scope)
		   (action    (eql :detached)))
  (unref-group (connector-connection connector) (scope->group scope)))

(defmethod receive-message ((connector in-connector)
			    (block?    t))
  "Delegate receiving a message to the connection of CONNECTOR."
  (values
   (receive-message (connector-connection connector) block?)
   :undetermined))

(defmethod message->event ((connector   in-connector)
			   (message     simple-array)
			   (wire-schema t))
  (let+ (((&accessors-r/o (pool      connector-assembly-pool)
			  (converter connector-converter)) connector)
	 (expose-wire-schema?  (connector-expose? connector :rsb.transport.wire-schema))
	 (expose-payload-size? (connector-expose? connector :rsb.transport.payload-size))
	 notification)

    ;; Try to unpack MESSAGE into a `notification' instance. Signal
    ;; `decoding-error' if that fails.
    (handler-bind
	((error #'(lambda (condition)
		    (error 'decoding-error
			   :encoded          message
			   :format-control   "~@<The data could not be ~
unpacked as a protocol buffer of kind ~S.~:@>"
			   :format-arguments '(fragmented-notification)
			   :cause            condition))))
      (setf notification (pb:unpack message 'fragmented-notification)))

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
			   :encoded          message
			   :format-control   "~@<After unpacking, the ~
notification~_~A~_could not be converted into an event.~:@>"
			   :format-arguments `(,(with-output-to-string (stream)
						  (describe notification stream)))
			   :cause            condition))))
      (notification->event pool converter notification
			   :expose-wire-schema?  expose-wire-schema?
			   :expose-payload-size? expose-payload-size?))))
