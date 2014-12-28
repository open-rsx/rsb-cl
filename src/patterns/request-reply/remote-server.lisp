;;;; remote-server.lisp --- The remote-server class is used to access a service.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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
           associated call information which is represented as a cons
           cell

             (CONDITION . RESULT).

           RESULT is initially nil and gets set when a call
           completes. CONDITION is a condition variable that is used
           to wait for RESULT."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Makes a method of a remote server callable by client code."))

(rsb::register-participant-class 'remote-method)

(defmethod initialize-instance :after ((instance remote-method)
                                       &key
                                       server)
  (closer-mop:set-funcallable-instance-function
   instance (lambda (data-or-event &rest args)
              (apply #'call server instance data-or-event args))))

(defmethod make-child-initargs ((participant remote-method)
                                (which       (eql nil))
                                (kind        t)
                                &key)
  (let* ((initargs  (call-next-method))
         (transform (getf initargs :transform)))
    (list* :transform (cdr (assoc (ecase kind
                                    (:listener :return)
                                    (:informer :argument))
                                  transform))
           (remove-from-plist initargs :transform))))

(defmethod make-child-initargs ((participant remote-method)
                                (which       (eql nil))
                                (kind        (eql :listener))
                                &key)
  (let+ ((initargs (call-next-method))
         ((&plist-r/o (filters :filters) (handlers :handlers)) initargs)
         ((&structure-r/o method- (lock %lock) (calls %calls)) participant)
         ((&flet handle-reply (event)
            (let+ (((&flet update-call (call)
                      (%call-result->future
                       participant (cadr call) event (cddr call) (car call)))))
              (cond
                ((when-let ((local-call *local-call*))
                   (cond
                     ((not (consp local-call))
                      nil)
                     ;; Can happen when more than one local-method
                     ;; processes a blocking local call.
                     ((eq (car local-call) t)
                      t)
                     (t
                      (update-call local-call)
                      (setf (car local-call) t)))))
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
    ;; Filter: ignore events which do have a suitable method to be
    ;; considered replies.
    ;;
    ;; Handler: `handle-reply' processes replies to method calls.
    (list* :filters  (list* *reply-filter* filters)
           :handlers (list* #'handle-reply handlers)
           (remove-from-plist initargs :filters :handlers))))

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

  (participant-child method nil :listener) ; force creation

  (let+ ((informer (participant-child method nil :informer))
         ((&structure-r/o method- (lock %lock) (calls %calls)) method)
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
  ((method-kind :allocation :class
                :initform   :remote-method))
  (:documentation
   "Allows client code to call methods provided by remote servers.

    The methods are presented as functions and can be called as if
    they were local."))

(rsb::register-participant-class 'remote-server)

(flet ((ensure-method (server name error? next-method)
         (or (funcall next-method server name :error? error?)
             (setf (participant-child server name :remote-method)
                   (make-child-participant server name :remote-method)))))
  (macrolet ((define-server-method-method (name-specializer)
               `(defmethod server-method ((server remote-server)
                                          (name   ,name-specializer)
                                          &key
                                          error?)
                  (ensure-method server name error? #'call-next-method))))
    (define-server-method-method string)
    (define-server-method-method (eql nil))))

(defmethod call ((server  remote-server)
                 (method  t)
                 (request t)
                 &rest args
                 &key &allow-other-keys)
  ;; Create the method named METHOD if it does not already
  ;; exist, then call it.
  (apply #'call server (server-method server method) request args))

(defmethod call ((server  t)
                 (method  remote-method)
                 (request t)
                 &rest args
                 &key &allow-other-keys)
  (let* ((informer (participant-child method nil :informer))
         (event    (make-instance 'event
                                  :scope (participant-scope informer)
                                  :data  request)))
    (apply #'call server method event args)))

;;; Utility functions

(declaim (ftype (function (event-id) (cons sequence-number string)) %event-id->key))

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
