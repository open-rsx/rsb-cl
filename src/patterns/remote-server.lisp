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

(defclass remote-method (method1)
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
  (:documentation
   "Instances of this class represent methods provided by a remote
server."))

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
		  (setf (cdr *local-call*)  event)
		  ;; Otherwise extract the call id, look up the call,
		  ;; store the result and notify the caller.
		  (let ((key (meta-data event :|rsb:reply|)))
		    (when key
		      (bt:with-lock-held (lock)
			(let ((call (gethash key calls)))
			  (when call
			    (setf (cdr call) event)
			    (bt:condition-notify (car call)))))))))
	  (rsb.ep:handlers new-value))))

(defmethod call ((server  t)
		 (method  remote-method)
		 (request event))
  "Call the remote method of METHOD transmitting REQUEST as request
data."
  (method-listener method) ;; force creation ;;; TODO(jmoringe): can we improve this?

  (bind (((:accessors-r/o (informer method-informer)
			  (lock     %method-lock)
			  (calls    %method-calls)) method)
	 (key)
	 (call         (cons nil nil))
	 (*local-call* call))
    ;; Send the request, wait for the reply and handle errors.
    (handler-case
	(progn
	  ;; Method has to be "REQUEST" for remote method calls.
	  (setf (event-method request) :|request|)

	  ;; Send the request to the remote server(s) and register the
	  ;; method call. We hold the lock the entire time to prevent
	  ;; the reply from arriving before we registered the call.
	  (bt:with-lock-held (lock)
	    (setf request (send informer request))

	    ;; If we already received the result via direct function
	    ;; calls, we do not have to generate an id and store the
	    ;; call.
	    (unless (cdr call)
	      (setf *local-call*        nil ;; reused to signal that local call did not happen
		    key                 (format nil "~(~A~)"
						(event-id request))
		    (car call)          (bt:make-condition-variable)
		    (gethash key calls) call)))

	  ;; Wait for the reply to arrive.
	  (if *local-call*
	      (event-data (cdr call)) ;;; TODO(jmoringe): handle remote errors
	      (sb-ext:with-timeout (server-timeout server)
		(bt:with-lock-held (lock)
		  (unwind-protect
		       (iter (until (cdr call))
			     (bt:condition-wait (car call) lock)
			     (finally
			      (let ((event (cdr call)))
				(if (meta-data event :|rsb:error?|)
				    (error 'remote-method-execution-error
					   :method  method
					   :request request
					   :cause   (make-condition
						     'simple-error
						     :format-control   "~@<~A~@:>"
						     :format-arguments (list (event-data event))))
				    (return (event-data event))))))
		    ;; Remove the call, even in case of errors or other
		    ;; non-local exits.
		    (remhash key calls))))))
      #+sbcl
      (sb-ext:timeout (condition)
	(declare (ignore condition))
	(error 'remote-call-timeout
	       :method  method
	       :request request))
      ((not remote-method-execution-error) (condition)
	(error 'remote-call-failed
	       :method  method
	       :request request
	       :cause   condition)))))

(defmethod call :around ((server  t)
			 (method  remote-method)
			 (request event))
  "TODO(jmoringe): document"
  (iter (restart-case
	    (return-from call (call-next-method))
	  (retry ()
	    :report (lambda (stream)
		      (format stream "~@<Retry calling method ~A of ~
server ~A with request ~A.~@:>"
			      method server request))))))


;;; `remote-server' class
;;

(defclass remote-server (server)
  ((timeout :initarg  :timeout
	    :type     positive-real
	    :accessor server-timeout
	    :initform 25
	    :documentation
	    "Stores the amount of seconds methods calls should wait
for their replies to arrive before failing."))
  (:documentation
   "Instances of this class represent remote servers in a way that
allows calling methods on them as if they were local."))

(define-lazy-creation-method remote-method listener ()  "reply")
(define-lazy-creation-method remote-method informer (t) "request")

(defmethod call ((server  remote-server)
		 (method  string)
		 (request t))
  "Create the method named METHOD if it does not already exist, then
call it."
  (let ((method (or (server-method server method :error? nil)
		    (setf (server-method server method)
			  (make-instance 'remote-method
					 :name method)))))
    (call server method request)))

(defmethod call ((server  t)
		 (method  remote-method)
		 (request t))
  (let ((event (make-instance 'event
			      :scope (participant-scope
				      (method-informer method))
			      :data  request)))
    (call server method event)))


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
