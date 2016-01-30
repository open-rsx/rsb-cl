;;;; local-introspection.lisp --- Classes and functions for local introspection.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;; `introspection-sender'

(defclass introspection-sender (participant-table-mixin
                                introspection-participant-mixin
                                rsb.ep:error-policy-mixin
                                rsb.ep:error-policy-handler-mixin
                                rsb.ep:restart-handler-mixin)
  ((process :accessor introspection-process
            :initform (current-process-info)
            :documentation
            "Stores information about the current process.")
   (host    :accessor introspection-host
            :initform (current-host-info)
            :documentation
            "Stores information about the local host."))
  (:documentation
   "Publishes introspection information for the current process.

    The published information describes the local host, the current
    process and participants existing within it."))

(defmethod initialize-instance :after ((instance introspection-sender)
                                       &key
                                       (participants '() participants-supplied?))
  ;; Forward processing errors to error hook.
  (setf (rsb.ep:processor-error-policy instance)
        (curry #'hooks:run-hook (participant-error-hook instance)))

  ;; Force creation of child participants.
  (participant-child instance :participants :listener)
  (participant-child instance :server       :local-server)

  ;; Broadcast initial hello messages for participants registered in
  ;; INSTANCE.
  (when participants-supplied?
    (setf (introspection-participants instance) participants)))

(let+ (((&flet reinitialize-server (introspection)
          (setf (participant-child introspection :server :local-server) nil)
          (participant-child introspection :server :local-server))))
  (defmethod (setf introspection-host) :after ((new-value     t)
                                               (introspection introspection-sender))
    (reinitialize-server introspection))
  (defmethod (setf introspection-process) :after ((new-value     t)
                                                  (introspection introspection-sender))
    (reinitialize-server introspection)))

(defmethod make-child-initargs ((participant introspection-sender)
                                (which       (eql :participants))
                                (kind        (eql :listener))
                                &key)
  (let+ ((initargs (call-next-method))
         ((&plist-r/o (filters :filters) (handlers :handlers)) initargs))
    ;; Use appropriate filters and install PARTICIPANT as handler
    ;; which processes queries.
    (list* :filters  (list* *survey-filter* filters)
           :handlers (list* participant handlers)
           (remove-from-plist initargs :filters :handlers))))

(defmethod make-child-scope ((participant introspection-sender)
                             (which       (eql :server))
                             (kind        (eql :local-server)))
  (let+ (((&accessors-r/o
           (scope                                     participant-scope)
           ((&structure-r/o process-info- process-id) introspection-process)
           ((&structure-r/o host-info- (host-id id))  introspection-host))
          participant))
    (introspection-process-scope process-id host-id scope)))

(defmethod make-child-participant ((participant introspection-sender)
                                   (which       (eql :server))
                                   (kind        (eql :local-server))
                                   &key)
  (let ((server (call-next-method)))
    (setf (server-method server "echo" :argument :event)
          (lambda (event)
            (make-event
             (event-scope event) (event-data event)
             :timestamps `(:request.send    ,(timestamp event :send)
                           :request.receive ,(timestamp event :receive))))
          (server-method server "eval")
          (lambda (request)
            (with-standard-io-syntax
              (write-to-string (eval (read-from-string request))))))
    server))

(defmethod (setf find-participant) ((new-value participant)
                                    (id        uuid:uuid)
                                    (container introspection-sender)
                                    &key
                                    parent-id
                                    if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (let+ (((&structure-r/o participant- kind scope) new-value))
    (setf (find-participant id container)
          (make-instance 'remote-participant-info
                         :kind       kind
                         :id         id
                         :parent-id  parent-id
                         :scope      scope
                         :type       (typecase new-value
                                       (informer (rsb::informer-type new-value))
                                       (t        t))
                         :transports (transport-specific-urls new-value)))))

(defmethod (setf find-participant) :after ((new-value remote-participant-info)
                                           (id        uuid:uuid)
                                           (container introspection-sender)
                                           &key
                                           parent-id
                                           if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (send container :hello :participant new-value))

(defmethod (setf find-participant) :before ((new-value (eql nil))
                                            (id        uuid:uuid)
                                            (container introspection-sender)
                                            &key
                                            parent-id
                                            if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (when-let ((participant (find-participant id container
                                            :if-does-not-exist nil)))
    (send container :bye :participant participant)))

(defmethod (setf find-participant) :around ((new-value (eql nil))
                                            (id        uuid:uuid)
                                            (container introspection-sender)
                                            &key
                                            parent-id
                                            if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (let ((old (introspection-participants container))
        (new (progn
               (call-next-method)
               (introspection-participants container))))
   (when (and (not (emptyp old)) (emptyp new))
     (log:info "~@<~A has no more participants; suiciding~@:>" container)
     (detach container))))

;; Query processing

(defmethod rsb.ep:handle ((sink introspection-sender)
                          (data event))
  (let+ (((&structure-r/o event- scope method (payload data)) data)
         (participant-id (handler-case
                             (scope->participant-id-or-nil
                              scope (participant-scope sink))
                           (error (condition)
                             (introspection-error
                              :request data condition
                              "~@<Malformed participant scope ~A~@:>"
                              (scope-string scope)))))
         ((&flet process (thunk)
            (cond
              ((and participant-id (eq method :|request|))
               (when-let ((participant (find-participant
                                        participant-id sink
                                        :if-does-not-exist nil)))
                 (funcall thunk :in-reply-to data :participant participant)))

              ((and (not participant-id) (eq method :|survey|))
               (mapc (curry thunk :in-reply-to data :participant)
                     (introspection-participants sink)))

              (t
               (introspection-error
                :request data nil
                "~@<~:[No participant id~:;Participant id ~:*~A~] in ~
                 scope ~A but ~:[no method~:;method ~:*~A~].~@:>"
                 participant-id (scope-string scope) method))))))
    (log:info "~@<~A received introspection ~:[survey~;query for ~:*~A~]: ~A~@:>"
              sink participant-id payload)
    (cond
      ((eq payload rsb.converter:+no-value+)
       (process (curry #'send sink :hello)))

      ((equal payload "ping")
       (process (curry #'send sink "pong")))

      (t
       (introspection-error
        :request data nil
        "~@<Payload is ~S (not ~{~S~^ or ~})~@:>"
        payload '(rsb.converter:+no-value+ "ping"))))))

;; Sending events

(defmethod send ((informer introspection-sender)
                 (data     t)
                 &key
                 (participant (missing-required-argument :participant))
                 in-reply-to)
  (declare (type remote-participant-info participant)
           (type (or null event)         in-reply-to))
  (let+ ((informer1 (participant-child informer :participants :informer))
         (scope     (participant-id->scope (participant-info-id participant)
                                           (participant-scope informer)))
         (event     (apply #'make-event scope data
                           (when in-reply-to
                             (list :causes (list (event-id/opaque in-reply-to)))))))
    (send informer1 event)))

(defmethod send ((informer introspection-sender)
                 (data     (eql :hello))
                 &rest args &key
                 (participant (missing-required-argument :participant)))
  (declare (type remote-participant-info participant))
  (let+ (((&structure-r/o introspection- host process) informer)
         (hello (make-instance 'hello
                               :participant participant
                               :process     process
                               :host        host)))
    (apply #'send informer hello args)))

(defmethod send ((informer introspection-sender)
                 (data     (eql :bye))
                 &rest args &key
                 (participant (missing-required-argument :participant)))
  (declare (type remote-participant-info participant))
  (let ((bye (make-instance 'bye :id (participant-info-id participant))))
    (apply #'send informer bye args)))

;;; `local-introspection'

(defclass local-introspection (introspection-sender
                               lockable-database-mixin)
  ()
  (:documentation
   "Broadcasts introspection information regarding the current process.

    The published information describes the local host, the current
    process and participants existing in the current process."))

(rsb::register-participant-class 'local-introspection)

(defmethod rsb.ep:handle ((sink local-introspection)
                          (data event))
  (with-database-lock (sink) (call-next-method)))
