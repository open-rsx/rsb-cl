;;;; mixins.lisp --- Mixins for introspection-related classes.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; `introspection-participant-mixin'

(defclass introspection-participant-mixin (participant)
  ((listener          :accessor introspection-%listener ; without lazy creation
                      :reader   introspection-listener  ; with lazy creation
                      :initform nil
                      :documentation
                      "Stores the listener participant used to receive
                       events by the participant; nil while the
                       listener has not been created.")
   (informer          :accessor introspection-%informer ; without-lazy creation
                      :reader   introspection-informer  ; with lazy creation
                      :initform nil
                      :documentation
                      "Stores the informer participant used to send
                       events by the participant; nil while the
                       informer has not been created.")
   (server            :accessor introspection-%server ; without lazy creation
                      :reader   introspection-server  ; with lazy creation
                      :initform nil
                      :documentation
                      "Stores a `remote-server' or `local-server' used
                       for making or serving requests for
                       host/process-level information.")
   (transport-options :initarg :transport-options
                      :type    list
                      :reader  participant-transport-options
                      :documentation
                      "Stores the transport options that should be
                       passed to the child participants when they are
                       created.

                       The stored options have already been merged
                       with defaults and thus make created child
                       participants ignore any special binding of
                       `*configuration*'."))
  (:default-initargs
   :scope             +introspection-scope+
   :transport-options (missing-required-initarg
                       'introspection-participant-mixin :transport-options))
  (:documentation
   "This class is intended to be mixed into introspection participant
    classes that have to send and receive events."))

(defmethod make-participant-using-class
    ((class     class)
     (prototype introspection-participant-mixin)
     (scope     scope)
     &rest args &key
     (transports '()))
  ;; TODO This logic can move into some of the composite patterns
  ;; after the 0.11 release.
  ;;
  ;; "Freeze" transport options by merging with defaults, thereby
  ;; disabling further inheritance from special bindings of
  ;; `*configuration*' during child participant creation (may even
  ;; happen in a different thread with different special bindings).
  ;;
  ;; Also check that TRANSPORTS will select at least one transport.
  (let ((transport-options (rsb::merge-transport-options
                            transports (transport-options))))
    (unless (rsb::effective-transport-options transport-options)
      (error 'no-transports-error
             :kind  (participant-kind prototype)
             :scope scope))

    (apply #'call-next-method class prototype scope
           :transport-options transports
           (remove-from-plist args :transports))))

(defun participant-make-child (participant kind scope
                               &rest initargs &key
                               (converters *introspection-all-converters*)
                               &allow-other-keys)
  (let+ (((&structure-r/o participant- error-hook transport-options)
          participant))
    (apply #'make-participant kind scope
           :transports     transport-options
           :converters     converters
           :error-policy   (lambda (condition)
                             (hooks:run-hook error-hook condition))
           :introspection? nil
           (remove-from-plist initargs :converters))))

(macrolet
    ((define-lazy-creation-method (kind)
       (let ((method-name   (symbolicate '#:introspection- kind))
             (accessor-name (symbolicate '#:introspection-% kind)))
         `(defmethod ,method-name :before ((introspection introspection-participant-mixin))
            (unless (,accessor-name introspection)
              (setf (,accessor-name introspection)
                    (participant-make-child
                     introspection ,kind
                     (introspection-participants-scope
                      (participant-scope introspection)))))))))

  (define-lazy-creation-method :listener)
  (define-lazy-creation-method :informer))

(defmethod detach ((participant introspection-participant-mixin))
  (when-let ((listener (introspection-%listener participant)))
    (detach listener))
  (when-let ((informer (introspection-%informer participant)))
    (detach informer))
  (when-let ((server (introspection-%server participant)))
    (detach server)))

(defmethod transport-specific-urls ((component introspection-participant-mixin))
  (when-let ((listener (introspection-%listener component)))
    (mapcar (curry #'puri:merge-uris (relative-url component))
            (transport-specific-urls listener))))

;;; `participant-table-mixin'

(defclass participant-table-mixin ()
  ((participants :type     hash-table
                 :accessor introspection-%participants
                 :initform (make-hash-table :test #'equalp)
                 :documentation
                 "Stores a mapping of participant ids to
                  `participant-info' instances."))
  (:documentation
   "This mixin class adds a table of `participant-info' instances
    indexed by id."))

(defmethod print-items:print-items append ((object participant-table-mixin))
  `((:num-participants ,(length (introspection-participants object)) " (~D)")))

(defmethod introspection-participants ((container participant-table-mixin))
  (hash-table-values (introspection-%participants container)))

(defmethod (setf introspection-participants) ((new-value sequence)
                                              (container participant-table-mixin))
  (map nil (lambda (participant)
             (setf (find-participant (participant-id participant) container)
                   participant))
       new-value))

(defmethod find-participant ((id        uuid:uuid)
                             (container participant-table-mixin)
                             &key
                             parent-id
                             (if-does-not-exist #'error))
  (declare (ignore parent-id))
  (let+ (((&structure-r/o introspection- %participants) container)
         (key (uuid:uuid-to-byte-array id)))
    (or (gethash key %participants)
        (error-behavior-restart-case
          (if-does-not-exist
           (no-such-participant-error :container container :id id))))))

(defmethod (setf find-participant) ((new-value t)
                                    (id        uuid:uuid)
                                    (container participant-table-mixin)
                                    &key
                                    parent-id
                                    if-does-not-exist)
  (declare (ignore parent-id if-does-not-exist))
  (let+ (((&structure-r/o introspection- %participants) container)
         (key (uuid:uuid-to-byte-array id)))
    (setf (gethash key %participants) new-value)))

(defmethod (setf find-participant) ((new-value (eql nil))
                                    (id        uuid:uuid)
                                    (container participant-table-mixin)
                                    &key
                                    parent-id
                                    (if-does-not-exist #'error))
  (declare (ignore parent-id))
  (let+ (((&structure-r/o introspection- %participants) container)
         (key (uuid:uuid-to-byte-array id)))
    (if (gethash key %participants)
        (remhash key %participants)
        (error-behavior-restart-case
            (if-does-not-exist
             (simple-error ; TODO condition
              :format-control   "~@<Cannot remove unknown participant ~
                                 with id ~A~@:>"
              :format-arguments (list id))
             :warning-condition simple-warning)))))

(defmethod ensure-participant ((id          uuid:uuid)
                               (container   participant-table-mixin)
                               (participant t))
  (or (find-participant id container :if-does-not-exist nil)
      (setf (find-participant id container) participant)))

(defmethod ensure-participant ((id          uuid:uuid)
                               (container   participant-table-mixin)
                               (participant cons)) ; (CLASS . INITARGS)
  (let ((existing (find-participant id container :if-does-not-exist nil)))
    (typecase existing
      (null
       (setf (find-participant id container)
             (apply #'make-instance participant)))
      (participant-entry-proxy
       (setf (find-participant id container)
             (apply #'change-class existing participant)))
      (t
       existing))))

;;; `lockable-database-mixin'

(defclass lockable-database-mixin ()
  ((lock :reader   database-%lock
         :initform (bt:make-lock "Database lock")))
  (:documentation
   "This mixins add a lock for use in introspection database
    classes."))

(defmethod call-with-database-lock ((database lockable-database-mixin)
                                    (thunk    function))
  (bt:with-lock-held ((database-%lock database))
    (funcall thunk)))

(defmacro with-database-lock ((database) &body body)
  `(call-with-database-lock ,database (lambda () ,@body)))

;;; `change-hook-mixin'

(defclass change-hook-mixin ()
  ((change-hook :type     list
                :initform '()
                :documentation
                "Stores a list of handlers to run when something
                 changes in the object."))
  (:documentation
   "This class is intended to be mixed into database classes that
    notify handlers of changes."))

(defmethod database-change-hook ((database change-hook-mixin))
  (hooks:object-hook database 'change-hook))
