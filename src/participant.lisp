;;;; participant.lisp --- A superclass for participant classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass participant (uuid-mixin
                       scope-mixin
                       converters-mixin
                       error-hook-mixin)
  ((id         :reader   participant-id)
   (scope      :reader   participant-scope)
   (transform  :initarg  :transform
               :reader   participant-transform
               :initform nil
               :documentation
               "Stores the transform which should be applied to
                processed events."))
  (:documentation
   "Instances of this class participate in the exchange of
    notifications on one channel of the bus."))

(defmethod relative-url ((participant participant))
  (puri:merge-uris
   (make-instance 'puri:uri
                  :fragment (prin1-to-string
                             (participant-id participant)))
   (relative-url (participant-scope participant))))

(defmethod print-object ((object participant) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A" (scope-string (participant-scope object)))))

;;; Participant creation

(defmethod make-participant ((class t) (scope scope) &rest args &key)
  (let ((class (find-class class)))
    (closer-mop:finalize-inheritance class)
    (apply #'make-participant-using-class
           class (closer-mop:class-prototype class) scope
           args)))

(defmethod make-participant-using-class :around ((class     class)
                                                 (prototype rsb.ep:client)
                                                 (scope     scope)
                                                 &rest args &key
                                                 (transports (transport-options))
                                                 (converters (default-converters)))
  (apply #'call-next-method class prototype scope
         :transports transports
         :converters converters
         (remove-from-plist args :transports :converters)))

(defmethod make-participant-using-class ((class     class)
                                         (prototype rsb.ep:client)
                                         (scope     scope)
                                         &rest args &key
                                         (direction (participant-direction prototype))
                                         transports
                                         converters
                                         transform)
  ;; Signal an error if no transports have been supplied.
  (unless transports
    (error 'no-transports-error
           :kind  (participant-kind prototype)
           :scope scope))

  ;; Replace &inherit marker in transport options with actual default
  ;; options for respective transports.
  (setf transports (map 'list #'process-transport-options transports))

  ;; Ensure that CONVERTERS is an alist of items of the form
  ;; (WIRE-TYPE . CONVERTER).
  (unless (and (listp converters) (consp (first converters)))
    (setf converters (list (cons t converters))))

  (let* ((configurator (make-instance
                        (ecase direction
                          ((:in-push :in-pull) 'rsb.ep:in-route-configurator)
                          (:out                'rsb.ep:out-route-configurator))
                        :scope     scope
                        :direction direction
                        :transform transform))
         (connectors   (rsb.transport:make-connectors
                        transports direction converters))
         (participant  (apply #'make-instance class
                              :scope        scope
                              :converters   converters
                              :configurator configurator
                              (remove-from-plist
                               args :direction :transports :converters)))
         (error-hook   (participant-error-hook participant)))

    ;; Associate constructed CONNECTORS to CONFIGURATOR instance.
    (setf (rsb.ep:configurator-connectors configurator) connectors)

    ;; Setup the error hook of PARTICIPANT to be run for all errors
    ;; intercepted by CONFIGURATOR.
    (setf (rsb.ep:processor-error-policy configurator)
          (lambda (condition)
            (hooks:run-hook error-hook condition)))

    participant))

(defmacro define-participant-creation-uri-methods (kind &rest args)
  (let* ((make-name      (symbolicate "MAKE-" kind))
         (arg-names      (mapcar (compose #'first #'ensure-list) args))
         (designator-arg (first arg-names)))
    ;; We want the generated method to be specialized on URI
    ;; designators.
    (unless (eq (second (first args)) 'puri:uri)
      (error "~@<The specializer of the first parameter is ~S, but ~
              should be ~S.~@:>"
             (second (first args)) 'puri:uri))

    `(progn
       ;; This method operates on URIs.
       (defmethod ,make-name
           (,@args
            &key
            (transports (transport-options
                         :exclude-disabled?
                         (not (uri-transport ,designator-arg))))
            (converters (default-converters))
            transform
            error-policy)
         (let+ (((&values scope options)
                 (uri->scope-and-options ,designator-arg transports)))
           (,make-name scope ,@(rest arg-names)
                       :transports   options
                       :converters   converters
                       :transform    transform
                       :error-policy error-policy)))

       ;; This method operates on strings which it turns into either
       ;; URIs (if the string contains a colon) or scopes.
       (defmethod ,make-name ((,designator-arg string) ,@(rest args)
                              &key
                              (transports nil transports-supplied?)
                              (converters nil converters-supplied?)
                              transform
                              error-policy)
         (apply #',make-name
                (if (find #\: ,designator-arg)
                    (puri:parse-uri ,designator-arg)
                    (make-scope ,designator-arg))
                ,@(rest arg-names)
                :transform    transform
                :error-policy error-policy
                (append
                 (when transports-supplied?
                   (list :transports transports))
                 (when converters-supplied?
                   (list :converters converters))))))))

;; This method operates on URIs.
(defmethod make-participant ((designator t) (scope puri:uri)
                             &rest args &key
                             (transports (transport-options
                                          :exclude-disabled?
                                          (not (uri-transport scope))))
                             (converters (default-converters))
                             transform
                             error-policy)
  (let+ (((&values scope options) (uri->scope-and-options scope transports)))
    (apply #'make-participant designator scope
           :transports   options
           :converters   converters
           :transform    transform
           :error-policy error-policy
           args)))

;; This method operates on strings which it turns into either URIs (if
;; the string contains a colon) or scopes.
(defmethod make-participant ((kind t) (scope string)
                             &rest args &key
                             (transports nil transports-supplied?)
                             (converters nil converters-supplied?)
                             transform
                             error-policy)
  (apply #'make-participant kind (if (find #\: scope)
                                     (puri:parse-uri scope)
                                     (make-scope scope))
         :transform    transform
         :error-policy error-policy
         (append (when transports-supplied?
                   (list :transports transports))
                 (when converters-supplied?
                   (list :converters converters))
                 args)))

(defun call-with-participant-creation-restarts (participant-kind designator
                                                normal-thunk args-changed-thunk)
  (declare (type function normal-thunk args-changed-thunk))
  ;; We use NORMAL-THUNK and ARGS-CHANGED-THUNK in order to implement
  ;; the common case of not invoking any restarts efficiently.
  (let+ (((&values designator designator-kind label parser)
          (etypecase designator
            (puri:uri (values designator :uri   "URI"   #'puri:parse-uri))
            (scope    (values designator :scope "scope" #'make-scope))))
         ((&labels read-new-value ()
            (format *query-io* "Specify ~A (not evaluated): " label)
            (force-output *query-io*)
            (list (funcall parser (read-line *query-io*)))))
         ((&labels report (stream)
            (format stream "~@<Retry creating the ~(~A~) with a ~
                            different ~A.~@:>"
                    participant-kind label))))
    (declare (dynamic-extent #'read-new-value #'report))
    (iter (with thunk = normal-thunk)
          (restart-case
              (return (funcall thunk designator))
            (retry ()
              :report (lambda (stream)
                        (format stream "~@<Retry creating the ~(~A~) ~
                                        for ~(~A~) ~S~@:>"
                                participant-kind designator-kind designator)))
            (use-uri (new-value)
              :test (lambda (condition)
                      (declare (ignore condition))
                      (eq designator-kind :uri))
              :interactive read-new-value
              :report report
              (setf designator new-value
                    thunk      args-changed-thunk))
            (use-scope (new-value)
              :test (lambda (condition)
                      (declare (ignore condition))
                      (eq designator-kind :scope))
              :interactive read-new-value
              :report report
              (setf designator new-value
                    thunk      args-changed-thunk))))))

(defmethod make-participant :around ((kind  t)
                                     (scope scope)
                                     &rest args &key)
  ;; Install restarts around the creation attempt.
  (flet ((make-participant-normal-thunk (scope)
           (declare (ignore scope))
           (call-next-method))
         (make-participant-args-changed-thunk (scope)
           (apply #'make-participant kind scope args)))
    (declare (dynamic-extent #'make-participant-normal-thunk
                             #'make-participant-args-changed-thunk))
    (call-with-participant-creation-restarts
     kind scope
     #'make-participant-normal-thunk
     #'make-participant-args-changed-thunk)))

(defmethod make-participant :around ((kind  t)
                                     (scope puri:uri)
                                     &rest args &key)
  ;; Install restarts around the creation attempt.
  (flet ((make-participant-normal-thunk (scope)
           (declare (ignore scope))
           (call-next-method))
         (make-participant-args-changed-thunk (scope)
           (apply #'make-participant kind scope args)))
    (declare (dynamic-extent #'make-participant-normal-thunk
                             #'make-participant-args-changed-thunk))
    (call-with-participant-creation-restarts
     kind scope
     #'make-participant-normal-thunk
     #'make-participant-args-changed-thunk)))

(defmacro define-participant-creation-restart-method (kind &rest args)
  "Emit an :around method on `make-KIND' that establishes restarts.
KIND will usually be one of :informer, :listener and :reader. ARGS is
a method lambda-list. The first argument is assumed to be designator
that is the URI or scope."
  (let* ((make-name      (symbolicate '#:make- kind))
         (arg-names      (mapcar (compose #'first #'ensure-list) args))
         (designator-arg (first arg-names)))
    (with-unique-names (args-var)
      `(defmethod ,make-name :around (,@args
                                      &rest ,args-var
                                      &key &allow-other-keys)
         ;; Install restarts around the creation attempt.
         (flet ((participant-creation-normal-thunk (,designator-arg)
                  (declare (ignore ,designator-arg))
                  (call-next-method))
                (participant-creation-args-changed-thunk (,designator-arg)
                  (apply #',make-name ,@arg-names ,args-var)))
           (declare (dynamic-extent #'participant-creation-normal-thunk
                                    #'participant-creation-args-changed-thunk))
           (call-with-participant-creation-restarts
            ',kind ,designator-arg
            #'participant-creation-normal-thunk
            #'participant-creation-args-changed-thunk))))))

(defmethod make-participant :around ((kind t) (scope t)
                                     &key
                                     transports)
  ;; Translate different kinds of errors into
  ;; `participant-creation-error' errors.
  (with-condition-translation
      (((error participant-creation-error)
        :kind       (make-keyword kind)
        :scope      scope
        :transports transports)) ; TODO not always available
    (call-next-method)))
