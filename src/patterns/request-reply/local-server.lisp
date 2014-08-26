;;;; local-server.lisp --- The local-server class is used to provide a service.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; `local-method' class

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
   "Method within a local server. Runs client code when called.

    The actual behavior of methods is implemented by invoking
    arbitrary user-supplied functions."))

(rsb::register-participant-class 'local-method)

(defmethod shared-initialize :after ((instance   local-method)
                                     (slot-names t)
                                     &key)
  (method-listener instance)) ; force creation

(define-lazy-creation-method local-method listener :argument)
(define-lazy-creation-method local-method informer :return)

(defmethod (setf method-%listener) :after ((new-value t)
                                           (method    local-method))
  ;; Ignore events which do have a suitable method to be considered
  ;; requests.
  (pushnew *request-filter* (receiver-filters new-value))

  ;; Install a handler on the request listener that calls the callback
  ;; and sends the reply using the informer.
  (push (curry #'call (method-server method) method)
        (rsb.ep:handlers new-value)))

(defmethod call :around ((server  t)
                         (method  local-method)
                         (request event)
                         &key &allow-other-keys)
  (if (eq *local-call* t)
      (bt:make-thread (lambda ()
                        (let ((*local-call* nil))
                          (call-next-method))))
      (call-next-method)))

(defmethod call ((server  t)
                 (method  local-method)
                 (request event)
                 &key &allow-other-keys)
  ;; Invoke the call back function of METHOD with the payload of
  ;; REQUEST. Send the result or an error notification back to the
  ;; caller.
  (let+ (((&structure-r/o method- informer callback argument) method)
         (causes (list (event-id/opaque request)))
         ((&flet make-reply (payload)
            (make-event (event-scope request) payload))))
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
          (send informer (make-reply result)
                :method :|reply|
                :causes causes))
      (error (condition)
        (send informer (make-reply (princ-to-string condition))
              :method       :|reply|
              :causes       causes
              :|rsb:error?| "1")))))

;;; `local-server' class

(defclass local-server (server)
  ()
  (:documentation
   "Makes a set of named functions available for remote invocation.

    Instances of this class associate a collection of `local-method'
    instances which are implemented by callback functions with a scope
    under which these methods are exposed for remote clients."))

(rsb::register-participant-class 'local-server)

(flet ((set-method (server scope name argument callback)
         (check-type argument argument-style)

         (setf (server-method server name)
               (make-participant :local-method scope
                                 :server   server
                                 :name     name
                                 :callback callback
                                 :argument argument))))

  (macrolet
      ((define-method-creating-method (name-type scope-form)
         `(defmethod (setf server-method) ((new-value function)
                                           (server    local-server)
                                           (name      ,name-type)
                                           &key
                                           (argument :payload))
            (let+ (((&structure-r/o participant- scope) server))
              (set-method server ,scope-form name argument new-value)))))

    (define-method-creating-method string
      (merge-scopes (list name) scope))
    (define-method-creating-method (eql nil)
      scope)))

;;; `local-server' creation

(defmethod make-local-server ((scope scope)
                              &key
                              (transports (transport-options))
                              (converters (default-converters))
                              transform
                              error-policy)
  "Make and return a `local-server' instance that provides a service
at the scope SCOPE."
  (make-participant :local-server scope
                    :converters   converters
                    :transports   transports
                    :transform    transform
                    :error-policy error-policy))
