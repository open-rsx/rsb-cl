;;;; remote-server.lisp --- The remote-server class is used to access a service.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; `remote-method' class

(defclass remote-method (method1
                         closer-mop:funcallable-standard-object)
  ((lock  :reader   method-%lock
          :initform (bt:make-lock "Remote Method Lock")
          :documentation
          "Stores a lock which protects the table of in-progress calls
from concurrent modification.")
   (calls :type     hash-table
          :reader   method-%calls
          :initform (make-hash-table :test #'equal)
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
   instance (lambda (data-or-event &rest args)
              (apply #'call (method-server instance) instance data-or-event
                     args))))

(define-lazy-creation-method remote-method listener :return   ())
(define-lazy-creation-method remote-method informer :argument (t))

(defmethod (setf method-%listener) :after ((new-value t)
                                           (method    remote-method))
  ;; Ignore events which do have a suitable method to be considered
  ;; replies.
  (pushnew *reply-filter* (receiver-filters new-value))

  ;; After instantiating the listener for METHOD, install a handler
  ;; for replies to method calls.
  (let+ (((&accessors-r/o (lock  method-%lock)
                          (calls method-%calls)) method)
         ((&flet handle-reply (event)
            (let+ (((&flet update-call (call)
                      (%call-result->future
                       method (cadr call) event (cddr call) (car call)))))
              (cond
                ((when-let ((local-call *local-call*))
                   (when (consp local-call)
                     (update-call local-call)
                     (setf (car local-call) t))))
                (t
                 ;; Extract the call id, look up the call, store the
                 ;; result and notify the caller.
                 (when-let* ((key (%event-id->key (first (event-causes event))))
                             ;; Find and maybe remove the call. Then
                             ;; store the received reply in the result
                             ;; future.
                             (call (bt:with-lock-held (lock)
                                     (when-let ((call (gethash key calls)))
                                       (remhash key calls)
                                       call))))
                   (update-call call))))))))
    (push #'handle-reply (rsb.ep:handlers new-value))))

(defmethod call ((server  t)
                 (method  remote-method)
                 (request event)
                 &key
                 (return :payload)
                 (block? t)
                 &allow-other-keys)
  ;; Call the remote method of METHOD transmitting REQUEST as request
  ;; data.
  (check-type return return-style "either :payload or :event")

  (method-listener method) ; force creation

  (let+ (((&accessors-r/o (informer method-informer)
                          (lock     method-%lock)
                          (calls    method-%calls)) method)
         (future       (make-instance 'future))
         (call         (cons future (cons request return)))
         (*local-call* (if block? call t)))
    (handler-case
        ;; Send the request to the remote server(s) and register the
        ;; method call. We hold the lock the entire time to prevent
        ;; the reply from arriving before we registered the call.
        (bt:with-lock-held (lock)
          (setf (event-method request) :|request|
                request                (send informer request))
          (unless (and block? (eq (car call) t))
            (let ((key (%event-id->key (event-id/opaque request))))
              (setf (gethash key calls) call)))
          future)
      (error (condition)
        (error 'remote-call-error
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
      (let ((scope (merge-scopes (list name) (participant-scope server))))
        (setf (server-method server name)
              (make-participant 'remote-method scope
                                :name name)))))

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
                               transform
                               error-policy)
  (make-participant 'remote-server scope
                    :converters   converters
                    :transports   transports
                    :transform    transform
                    :error-policy error-policy))

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
