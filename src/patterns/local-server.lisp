;;; local-server.lisp --- The local-server class is used to provide a service.
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

(in-package :rsb.patterns)


;;; `local-method' class
;;

(defclass local-method (method1)
  ((callback :initarg  :callback
	     :type     function
	     :reader   method-callback
	     :documentation
	     "Stores the function that is called to perform the actual
processing of the method."))
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

(defmethod call :around ((server  t)
			 (method  local-method)
			 (request event))
  "Ignore the call if REQUEST does not have a request id."
  (if (meta-data request :|ServerRequestId|)
      (call-next-method)
      (warn "~@<Received a request without id: ~A.~@:>"
	    request)))

(defmethod call ((server  t)
		 (method  local-method)
		 (request event))
  "Invoke the call back function of METHOD with the payload of
REQUEST. Send the result or an error notification back to the caller."
  (bind (((:accessors-r/o (informer method-informer)
			  (callback method-callback)) method)
	 (id (meta-data request :|ServerRequestId|))
	 ((:values result error?)
	  (handler-case
	      (funcall callback (event-data request))
	    (error (condition)
	      (values (format nil "~A" condition) t)))))
    (apply #'send informer result
	   :|ServerRequestId| id
	   (when error? (list :|isException| "yes")))))


;;; `local-server' class
;;

(defclass local-server (server)
  ()
  (:documentation
   "Instances of this class associate a collection of method instances
which are implemented by callback functions with a scope under which
these methods are exposed for remote clients."))

(define-lazy-creation-method local-method listener ()  "request")
(define-lazy-creation-method local-method informer (t) "reply")

(defmethod (setf server-method) ((new-value function)
				 (server    local-server)
				 (name      string))
  (setf (server-method server name)
	(make-instance 'local-method
		       :server   server
		       :name     name
		       :callback new-value)))


;;; `local-server' creation
;;

(defmethod make-local-server ((scope scope)
			      &key
			      (transports (rsb::transport-options)))
  "Make and return a `local-server' instance that provides a service
at the scope SCOPE."
  (make-instance 'local-server
		 :scope             scope
		 :transport-options transports))

(define-participant-creation-uri-methods local-server (scope puri:uri))

(define-participant-creation-restart-method local-server (scope scope))
(define-participant-creation-restart-method local-server (scope puri:uri))
