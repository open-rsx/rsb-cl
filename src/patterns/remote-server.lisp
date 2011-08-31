;;; remote-server.lisp --- The remote-server class is used to access a service.
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


;;; `remote-method' class
;;

(defclass remote-method (method1
			 closer-mop:funcallable-standard-object)
  ((lock  :initarg  :lock
	  :initform (bt:make-lock)
	  :reader   %method-lock
	  :documentation
	  "Stores a lock which protects the table of in-progress calls
from concurrent modification.")
   (calls :initarg  :calls
	  :type     hash-table
	  :initform (make-hash-table :test #'equal)
	  :reader   %method-calls
	  :documentation
	  "Stores a mapping of request ids (as strings) to the
associated call information which is represented as a cons cell
\(CONDITION . RESULT). RESULT is initially nil and gets set when a
call completes. CONDITION is a condition variable that is used to wait
for RESULT."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class represent methods provided by a remote
server."))

(defmethod initialize-instance :after ((instance remote-method)
                                       &key)
  (closer-mop:set-funcallable-instance-function
   instance
   #'(lambda (data-or-event &rest args)
       (apply #'call (method-server instance) instance data-or-event
	      args))))

(define-lazy-creation-method remote-method listener ()  "reply")
(define-lazy-creation-method remote-method informer (t) "request")

(defmethod (setf %method-listener) :after ((new-value t)
					   (method    remote-method))
  "After instantiating the listener for METHOD, install a handler for
replies to method calls."
  (bind (((:accessors-r/o (lock  %method-lock)
			  (calls %method-calls)) method))
    (push #'(lambda (event)
	      ;; Check whether this is a direct call within a single
	      ;; thread.
	      (if *local-call*
		  ;; If so, just store the result.
		  (progn
		    (%call-result->future
		     method (cadr *local-call*) event
		     (cddr *local-call*) *local-call*)
		    (setf (cdr *local-call*) nil))
		  ;; Otherwise extract the call id, look up the call,
		  ;; store the result and notify the caller.
		  (let ((key  (meta-data event :|rsb:reply|))
			(call))
		    (when key
		      (bt:with-lock-held (lock)
			(when (setf call (gethash key calls))
			  (remhash key calls)))
		      ;; Remove the call.
		      (when call
			(%call-result->future
			 method (cadr call) event
			 (cddr call) (car call)))))))
	  (rsb.ep:handlers new-value))))

(defmethod call ((server  t)
		 (method  remote-method)
		 (request event)
		 &key
		 (return :payload)
		 &allow-other-keys)
  "Call the remote method of METHOD transmitting REQUEST as request
data."
  (check-type return return-style "either :payload or :event")

  (method-listener method) ;; force creation ;;; TODO(jmoringe): can we improve this?

  (bind (((:accessors-r/o (informer method-informer)
			  (lock     %method-lock)
			  (calls    %method-calls)) method)
	 (*local-call* (cons nil (cons request return))))
    (handler-case
	;; Send the request to the remote server(s) and register the
	;; method call. We hold the lock the entire time to prevent
	;; the reply from arriving before we registered the call.
	(bt:with-lock-held (lock)
	  ;; Method has to be "REQUEST" for remote method calls.
	  (setf (event-method request) :|request|
		request                (send informer request))

	  ;; If we already received the result via direct function
	  ;; calls, we do not have to generate an id and store the
	  ;; call.
	  (if (null (cdr *local-call*))
	      *local-call*
	      (let ((future (make-instance 'future)))
		(setf (gethash (format nil "~(~A~)" (event-id request)) calls)
		      (cons future (cons request return)))
		future)))
      (error (condition)
	(error 'remote-call-failed
	       :method  method
	       :request request
	       :cause   condition)))))

(defmethod call :around ((server  t)
			 (method  remote-method)
			 (request event)
			 &key
			 (block?  t)
			 timeout
			 &allow-other-keys)
  "Establish restarts and take care retrieving future results if
BLOCK? is non-nil."
  (iter (restart-case
	    (return-from call
	      (if block?
		  (nth-value 0 (future-result (call-next-method)
					      :timeout timeout))
		  (call-next-method)))
	  (retry ()
	    :report (lambda (stream)
		      (format stream "~@<Retry calling method ~A of ~
server ~A with request ~A.~@:>"
			      method server request))))))


;;; `remote-server' class
;;

(defclass remote-server (server)
  ()
  (:documentation
   "Instances of this class represent remote servers in a way that
allows calling methods on them as if they were local."))

(defmethod server-method ((server remote-server)
			  (name   string)
			  &key
			  error?)
  (or (call-next-method server name :error? error?)
      (setf (server-method server name)
	    (make-instance 'remote-method
			   :name name))))

(defmethod call ((server  remote-server)
		 (method  string)
		 (request t)
		 &rest args
		 &key &allow-other-keys)
  "Create the method named METHOD if it does not already exist, then
call it."
  (apply #'call server (server-method server method) request args))

(defmethod call ((server  t)
		 (method  remote-method)
		 (request t)
		 &rest args
		 &key &allow-other-keys)
  (let ((event (make-instance 'event
			      :scope (participant-scope
				      (method-informer method))
			      :data  request)))
    (apply #'call server method event args)))


;;; `remote-server' creation
;;

(defmethod make-remote-server ((scope scope)
			       &key
			       (transports (rsb::transport-options))) ;;; TODO(jmoringe): package
  (make-instance 'remote-server
		 :scope             scope
		 :transport-options transports))

(define-participant-creation-uri-methods remote-server (scope puri:uri))

(define-participant-creation-restart-method remote-server (scope scope))
(define-participant-creation-restart-method remote-server (scope puri:uri))


;;; Utility functions
;;

(defun %call-result->future (method request event return future)
  "Store data from METHOD, REQUEST and EVENT in FUTURE taking into
account whether EVENT represents an error. Return the modified
FUTURE."
  (if (meta-data event :|rsb:error?|)
      (setf (future-error future)
	    (list 'remote-method-execution-error
		  :method  method
		  :request request
		  :cause   (make-condition
			    'simple-error
			    :format-control   "~@<~A~@:>"
			    :format-arguments (list (event-data event)))))
      (setf (future-result future) (ecase return
				     (:payload (event-data event))
				     (:event   event))))
  future)
