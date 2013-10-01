;;;; remote-server.lisp --- The remote-server class is used to access a service.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.patterns)

;;; `remote-method' class

(defclass remote-method (method1
                         closer-mop:funcallable-standard-object)
  ((lock  :initarg  :lock
          :initform (bt:make-lock "Remote Method Lock")
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

(define-lazy-creation-method remote-method listener :return   ()  "reply")
(define-lazy-creation-method remote-method informer :argument (t) "request")

(defmethod (setf %method-listener) :after ((new-value t)
                                           (method    remote-method))
  "After instantiating the listener for METHOD, install a handler for
replies to method calls."
  (let+ (((&accessors-r/o (lock  %method-lock)
                          (calls %method-calls)) method))
    (push #'(lambda (event)
              ;; Check whether this is a direct call within a single
              ;; thread.
              (if *local-call*
                  ;; If so, just store the result (unless that
                  ;; happened already).
                  (when (cdr *local-call*)
                    (%call-result->future
                     method (cadr *local-call*) event
                     (cddr *local-call*) *local-call*)
                    (setf (cdr *local-call*) nil))
                  ;; Otherwise extract the call id, look up the call,
                  ;; store the result and notify the caller.
                  (let ((key  (%event-id->key
                               (first (event-causes event))))
                        (call))
                    (when key
                      ;; Remove the call.
                      (bt:with-lock-held (lock)
                        (when (setf call (gethash key calls))
                          (remhash key calls)))
                      ;; Store the received reply in the result
                      ;; future.
                      (when call
                        (%call-result->future
                         method (cadr call) event (cddr call) (car call)))))))
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

  (method-listener method) ;; force creation ; TODO(jmoringe): can we improve this?

  (let+ (((&accessors-r/o (informer method-informer)
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
              (let ((future (make-instance 'future))
                    (key    (%event-id->key (event-id/opaque request))))
                (setf (gethash key calls)
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
  (check-type timeout (or null timeout))

  (iter (restart-case
            (return-from call
              (if block?
                  (let ((result (future-result (call-next-method)
                                               :timeout timeout)))
                    (if (eq result rsb.converter:+no-value+)
                        (values)
                        result))
                  (call-next-method)))
          (retry ()
            :report (lambda (stream)
                      (format stream "~@<Retry calling method ~A of ~
server ~A with request ~A.~@:>"
                              method server request))))))

;;; `remote-server' class

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

(defmethod make-remote-server ((scope scope)
                               &key
                               (transports (transport-options))
                               (converters (default-converters))
                               transform)
  (make-instance 'remote-server
                 :scope             scope
                 :converters        converters
                 :transport-options transports
                 :transform         transform))

(define-participant-creation-uri-methods remote-server (scope puri:uri))

(define-participant-creation-restart-method remote-server (scope scope))
(define-participant-creation-restart-method remote-server (scope puri:uri))

;;; Utility functions

(declaim (ftype (function (event-id) (cons sequence-number string)) %event-id->key)
         (inline %event-id->key))

(defun %event-id->key (event-id)
  "Return an `equall'-comparable object representing EVENT-ID."
  (cons (cdr event-id) (princ-to-string (car event-id))))

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
