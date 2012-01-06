;;; local-server.lisp --- The local-server class is used to provide a service.
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

(cl:in-package :rsb.patterns)


;;; `local-method' class
;;

(defclass local-method (method1)
  ((callback :initarg  :callback
	     :type     function
	     :reader   method-callback
	     :documentation
	     "Stores the function that is called to perform the actual
processing of the method.")
   (argument :initarg  :argument
	     :type     argument-style
	     :reader   method-argument
	     :initform :payload
	     :documentation
	     "Stores the kind of argument (event vs. payload) that
should be passed to the callback function."))
  (:default-initargs
   :callback (missing-required-initarg 'local-method :callback))
  (:documentation
   "Instances of this class implement and make available methods of a
local server. The actual behavior of methods is implemented by
invoking arbitrary user-supplied functions."))

(defmethod shared-initialize :after ((instance   local-method)
				     (slot-names t)
				     &key)
  "Install a handler on the request listener that calls the callback
and send the reply using the informer."
  (setf (rsb.ep:handlers (method-listener instance))
	(list (curry #'call (method-server instance) instance))))

(define-lazy-creation-method local-method listener ()  "request")
(define-lazy-creation-method local-method informer (t) "reply")

(defmethod call ((server  t)
		 (method  local-method)
		 (request event)
		 &key &allow-other-keys)
  "Invoke the call back function of METHOD with the payload of
REQUEST. Send the result or an error notification back to the caller."
  (let+ (((&accessors-r/o (informer method-informer)
			  (callback method-callback)
			  (argument method-argument)) method)
	 ;; If we got here via direct function calls within a single
	 ;; thread, the event id-based mechanism is not required.
	 (causes (unless *local-call*
		   (list (event-id/opaque request)))))
    (handler-case
	(let* ((maybe-result (multiple-value-list
			      (cond
				((eq argument :event)
				 (funcall callback request))
				((eq (event-data request) rsb.converter:+no-value+)
				 (funcall callback))
				(t
				 (funcall callback (event-data request))))))
	       (result       (if maybe-result
				 (first maybe-result)
				 rsb.converter:+no-value+)))
	  (send informer result
		:method :|reply|
		:causes causes))
      (error (condition)
	(send informer (format nil "~A" condition)
	      :method       :|reply|
	      :causes       causes
	      :|rsb:error?| "1")))))


;;; `local-server' class
;;

(defclass local-server (server)
  ()
  (:documentation
   "Instances of this class associate a collection of method instances
which are implemented by callback functions with a scope under which
these methods are exposed for remote clients."))

(defmethod (setf server-method) ((new-value function)
				 (server    local-server)
				 (name      string)
				 &key
				 (argument :payload))
  (check-type argument argument-style "either :payload or :event")

  (setf (server-method server name)
	(make-instance 'local-method
		       :server   server
		       :name     name
		       :callback new-value
		       :argument argument)))


;;; `local-server' creation
;;

(defmethod make-local-server ((scope scope)
			      &key
			      (transports (transport-options))
			      (converters (default-converters)))
  "Make and return a `local-server' instance that provides a service
at the scope SCOPE."
  (make-instance 'local-server
		 :scope             scope
		 :converters        converters
		 :transport-options transports))

(define-participant-creation-uri-methods local-server (scope puri:uri))

(define-participant-creation-restart-method local-server (scope scope))
(define-participant-creation-restart-method local-server (scope puri:uri))
