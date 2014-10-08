;;;; compat.lisp --- Backwards compatibility.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Conditions

(macrolet
    ((define-comatibility-type (kind)
       (let ((predicate-name (symbolicate kind '#:-creation-error?))
             (type-name      (symbolicate kind '#:-creation-error)))
         `(progn
            (defun ,predicate-name (condition)
              (and (typep condition 'participant-creation-error)
                   (eq ,kind (participant-creation-error-kind condition))))

            (deftype ,type-name ()
              ,(format nil "This error is signaled when an attempt to
                            create a ~(~A~) fails." kind)
              '(satisfies ,predicate-name))))))

  (define-comatibility-type :reader)
  (define-comatibility-type :listener)
  (define-comatibility-type :informer))

(defun informer-creation-error-type (condition)
  (declare (ignore condition))
  t)

;;; Creation methods

(defmacro define-participant-creation-uri-methods (kind &rest args)
  (let* ((make-name      (let ((*package* (symbol-package kind)))
                           (symbolicate '#:make- kind)))
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
       (defmethod ,make-name (,@args
                              &key
                              (transports '())
                              (converters (default-converters))
                              transform
                              error-policy)
         (let+ (((&values scope uri-transports)
                 (uri->scope-and-options ,designator-arg)))
           (,make-name scope ,@(rest arg-names)
                       :transports   (merge-transport-options
                                      uri-transports transports)
                       :converters   converters
                       :transform    transform
                       :error-policy error-policy)))

       ;; This method operates on strings which it turns into either
       ;; URIs (if the string contains a colon) or scopes.
       (defmethod ,make-name ((,designator-arg string) ,@(rest args)
                              &key
                              (transports  nil transports-supplied?)
                              (converters  nil converters-supplied?)
                              transform
                              error-policy)
         (apply #',make-name (parse-scope-or-uri ,designator-arg)
                ,@(rest arg-names)
                :transform    transform
                :error-policy error-policy
                (append
                 (when transports-supplied?
                   (list :transports transports))
                 (when converters-supplied?
                   (list :converters converters))))))))

(defmacro define-participant-creation-restart-method (kind &rest args)
  "Emit an :around method on `make-KIND' that establishes restarts.
KIND will usually be one of :informer, :listener and :reader. ARGS is
a method lambda-list. The first argument is assumed to be designator
that is the URI or scope."
  (let* ((make-name      (let ((*package* (symbol-package kind)))
                           (symbolicate '#:make- kind)))
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

;; `reader'

(define-participant-creation-uri-methods reader (scope puri:uri))

(define-participant-creation-restart-method reader (scope scope))
(define-participant-creation-restart-method reader (scope puri:uri))

;; `listener'

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))

;; `informer'

(define-participant-creation-uri-methods informer
    (scope puri:uri) (type t))

(define-participant-creation-restart-method informer
    (scope scope) (type t))
(define-participant-creation-restart-method informer
    (scope puri:uri) (type t))

;; `local-server'

(define-participant-creation-uri-methods
    rsb.patterns.request-reply:local-server (scope puri:uri))

(define-participant-creation-restart-method
    rsb.patterns.request-reply:local-server (scope scope))
(define-participant-creation-restart-method
    rsb.patterns.request-reply:local-server (scope puri:uri))

;; `remove-server'

(define-participant-creation-uri-methods
    rsb.patterns.request-reply:remote-server (scope puri:uri))

(define-participant-creation-restart-method
    rsb.patterns.request-reply:remote-server (scope scope))
(define-participant-creation-restart-method
    rsb.patterns.request-reply:remote-server (scope puri:uri))
