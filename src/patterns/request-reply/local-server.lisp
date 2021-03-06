;;;; local-server.lisp --- The local-server class is used to provide a service.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; `local-method' class

(defclass local-method (method1)
  ((server   :initarg  :server
             :type     local-server
             :reader   method-%server
             :documentation
             "Stores the `local-server' instance to which the method
              belongs.")
   (callback :initarg  :callback
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

(defmethod initialize-instance :after ((instance local-method) &key)
  (participant-child instance nil :listener)) ; force creation

(defmethod make-child-initargs ((participant local-method)
                                (which       (eql nil))
                                (kind        t)
                                &key)
  (let* ((initargs  (call-next-method))
         (transform (getf initargs :transform)))
    (list* :transform (cdr (assoc (ecase kind
                                    (:listener :argument)
                                    (:informer :return))
                                  transform))
           (remove-from-plist initargs :transform))))

(defmethod make-child-initargs ((participant local-method)
                                (which       (eql nil))
                                (kind        (eql :listener))
                                &key)
  (let+ ((initargs (call-next-method))
         ((&plist-r/o (filters :filters) (handlers :handlers)) initargs))
    ;; Filter: ignore events which do have a suitable method to be
    ;; considered requests.
    ;;
    ;; Handler: call the callback and sends the reply using the
    ;; informer.
    (list* :filters  (list* *request-filter* filters)
           :handlers (list* (curry #'call (method-%server participant) participant)
                            handlers)
           (remove-from-plist initargs :filters :handlers))))

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
  (let+ ((informer (participant-child method nil :informer))
         ((&structure-r/o method- callback argument) method)
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
               (result       (cond
                               ((not maybe-result)
                                rsb.converter:+no-value+)
                               ((typep maybe-result '(cons event null))
                                (first maybe-result))
                               (t
                                (make-reply (first maybe-result))))))
          (send informer result :method :|reply| :causes causes))
      (error (condition)
        (send informer (make-reply (princ-to-string condition))
              :method       :|reply|
              :causes       causes
              :|rsb:error?| "1")))))

;;; `local-server' class

(defclass local-server (server)
  ((method-kind :allocation :class
                :initform   :local-method))
  (:documentation
   "Makes a set of named functions available for remote invocation.

    Instances of this class associate a collection of `local-method'
    instances which are implemented by callback functions with a scope
    under which these methods are exposed for remote clients."))

(rsb::register-participant-class 'local-server)

(flet ((set-method (server name argument callback)
         (check-type argument argument-style)

         (setf (participant-child server name :local-method)
               (make-child-participant server name :local-method
                                       :callback callback
                                       :argument argument))))

  (macrolet
      ((define-setf-server-method-method (name-type)
         `(defmethod (setf server-method) ((new-value function)
                                           (server    local-server)
                                           (name      ,name-type)
                                           &key
                                           (argument :payload))
            (set-method server name argument new-value))))

    (define-setf-server-method-method string)
    (define-setf-server-method-method (eql nil))))
