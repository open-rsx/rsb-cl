;;;; participant.lisp --- A superclass for participant classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass participant (uuid-mixin
                       scope-mixin)
  ((id         :reader   participant-id)
   (scope      :reader   participant-scope)
   (converters :initarg  :converters
               :type     list
               :initform '()
               :reader   participant-converters
               :documentation
               "Stores a list of the converters available for use in
connectors of the participant. Each element is of the form

  (WIRE-TYPE . CONVERTER)

.")
   (transform  :initarg  :transform
               :reader   participant-transform
               :initform nil
               :documentation
               "Stores the transform which should be applied to
processed events.")
   (error-hook :type     list #|of function|#
               :initform '()
               :documentation
               "Stores a list of functions to call in case of
errors."))
  (:documentation
   "Instances of this class participate in the exchange of
notifications on one channel of the bus."))

(defmethod participant-converter ((participant participant)
                                  (wire-type   t)
                                  &key &allow-other-keys)
  "Return the converter for WIRE-TYPE that is used by the connectors
of PARTICIPANT."
  (mapcar #'cdr
          (remove wire-type (participant-converters participant)
                  :key      #'car
                  :test-not #'subtypep)))

(defmethod participant-error-hook ((participant participant))
  (hooks:object-hook participant 'error-hook))

(defmethod relative-url ((participant participant))
  (puri:merge-uris
   (make-instance 'puri:uri
                  :fragment (prin1-to-string
                             (participant-id participant)))
   (relative-url (participant-scope participant))))

(defmethod detach ((participant participant))
  "Let PARTICIPANT's configurator do the heavy lifting."
  (detach (rsb.ep:client-configurator participant)))

(defmethod print-object ((object participant) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A" (scope-string (participant-scope object)))))

;;; Participant creation

;; TODO(jmoringe): infer direction?

(defun make-participant (class scope direction
                         transports converters transform error-policy
                         &rest args)
  "Make and return a participant instance of CLASS that participates
in the channel designated by SCOPE.

DIRECTION is one of :in-push, :in-pull and :out.

TRANSPORTS is a list of connector classes.

CONVERTERS is an alist of converters for particular wire-types with
items of the form (WIRE-TYPE . CONVERTER).

When non-nil, TRANSFORM is a transform object usable with
`rsb.event-processing:transform!'.

ERROR-POLICY has to be nil or a function to be installed in the error
hook of the created participant.

ARGS are arguments for the created CLASS instance.

Return three values:
1) the `participant' instance
2) the associated `rsb.event-processing:configurator' instance
2) the list of instantiated `rsb.transport:connector's"
  ;; Signal an error if no transports have been supplied.
  (unless transports
    (error 'no-transports-error :scope scope))

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
                              args))
         (error-hook   (participant-error-hook participant)))

    ;; Associate constructed CONNECTORS to CONFIGURATOR instance.
    (setf (rsb.ep:configurator-connectors configurator) connectors)

    ;; Setup the error hook of PARTICIPANT to be run for all errors
    ;; intercepted by CONFIGURATOR.
    (setf (rsb.ep:processor-error-policy configurator)
          (lambda (condition)
            (hooks:run-hook error-hook condition)
            ;; TODO(jmoringe): maybe (ignore-error) here?
            ))
    (when error-policy
      (hooks:add-to-hook error-hook error-policy))

    (values participant configurator connectors)))

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

(defun call-with-participant-creation-restarts (participant-kind designator-kind
                                                designator
                                                normal-thunk args-changed-thunk)
  (declare (type function normal-thunk args-changed-thunk))
  ;; We use NORMAL-THUNK and ARGS-CHANGED-THUNK in order to implement
  ;; the common case of not invoking any restarts efficiently.
  (let+ ((designator designator)
         ((&values label parser) (ecase designator-kind
                                   (:uri   (values "URI"   #'puri:parse-uri))
                                   (:scope (values "scope" #'make-scope))))
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

(defmacro define-participant-creation-restart-method (kind &rest args)
  "Emit an :around method on `make-KIND' that establishes restarts.
KIND will usually be one of :informer, :listener and :reader. ARGS is
a method lambda-list. The first argument is assumed to be designator
that is the URI or scope."
  (let* ((make-name       (symbolicate '#:make- kind))
         (arg-names       (mapcar (compose #'first #'ensure-list) args))
         (designator-arg  (first arg-names))
         (designator-kind (make-keyword (second (first args)))))
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
            ',kind ,designator-kind ,designator-arg
            #'participant-creation-normal-thunk
            #'participant-creation-args-changed-thunk))))))
