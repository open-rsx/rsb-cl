;;;; server.lisp --- A superclass for local and remote server classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; `method1' class

(defclass method1 (participant)
  ((server   :initarg  :server
             :type     server
             :reader   method-server
             :writer   (setf method-%server)
             :documentation
             "Stores the server object to which the method belongs.")
   (name     :initarg  :name
             :type     (or null method-name)
             :reader   method-name
             :documentation
             "Stores the name of the method.")
   (informer :type     (or null informer)
             :accessor method-%informer ; without lazy creation
             :reader   method-informer  ; with lazy creation
             :initform nil
             :documentation
             "Stores the `informer' instance associated to the
method. The instance is created lazily when first used.")
   (listener :type     (or null listener)
             :accessor method-%listener ; without lazy creation
             :reader   method-listener  ; with lazy creation
             :initform nil
             :documentation
             "Stores the `listener' instance associated to the
method. The instance is created lazily when first used."))
  (:default-initargs
   :name (missing-required-initarg 'method1 :name))
  (:documentation
   "This class serves as a superclass for local and remote method
classes."))

(defmethod shared-initialize :before ((instance   method1)
                                      (slot-names t)
                                      &key
                                      (name nil name-supplied?))
  (when (and name-supplied? name)
    (check-type name method-name "a legal method name")))

(defmethod detach ((method method1))
  (let+ (((&accessors-r/o (informer method-%informer)
                          (listener method-%listener)) method))
    ;; For the sake of the `local-method' subclass: shutdown the
    ;; listener first. This will prevent new method calls from being
    ;; initiated and wait for in-progress calls to finish while still
    ;; having the informer available for sending out reply events.
    ;;
    ;; This shutdown sequence doesn't help `remote-method', but the
    ;; client should not detach a `remote-server' with in-progress
    ;; method calls and expect replies to arrive anyway.
    (when listener (detach listener))
    (when informer (detach informer))))

(defmethod print-object ((object method1) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (method-name object))))

(defmacro define-lazy-creation-method (class slot transform)
  "Define a :before method on the reader function for SLOT of CLASS
   that lazily creates the slot value.

   TRANSFORM has to be either :argument or :return and the
   corresponding transform is installed into the created participant."
  (let ((method-name   (symbolicate "METHOD-" slot))
        (accessor-name (symbolicate "METHOD-%" slot))
        (kind          (make-keyword slot)))
    `(defmethod ,method-name :before ((method ,class))
       ,(format nil "Lazily create the ~(~A~) when it is first requested."
                slot)
       (unless (,accessor-name method)
         (let+ (((&accessors-r/o (scope  participant-scope) ; TODO all this is not quite right yet
                                 (server method-server)) method)
                ((&structure-r/o participant- converters transform error-hook) server)
                (transform (cdr (assoc ,transform transform))))
           (setf (,accessor-name method)
                 (make-participant ,kind scope
                                   :transports     (server-transport-options server)
                                   :converters     converters
                                   :transform      transform
                                   :error-policy   (lambda (condition)
                                                     (hooks:run-hook
                                                      error-hook condition))
                                   :parent         method
                                   :introspection? (server-introspection? server))))))))

;;; `server' class

(defclass server (participant)
  ((transport-options :initarg  :transport-options
                      :type     list
                      :reader   server-transport-options
                      :initform '()
                      :documentation
                      "Stores the transport options that should be
                       used by participants which implement the actual
                       communication on behalf of the server.

                       The stored options have already been merged
                       with defaults and thus make created child
                       participants ignore any special binding of
                       `*configuration*'.")
   (introspection?    :initarg  :introspection?
                      :type     boolean
                      :reader   server-introspection?
                      :initform t
                      :documentation
                      "Stores a Boolean indicating whether
                       introspection has enabled for the server.

                       TODO this is a temporary workaround until
                       participant-configuration is ready.")
   (rsb::transform    :type     transform-specification)
   (methods           :type     hash-table
                      :reader   server-%methods
                      :initform (make-hash-table :test #'equal)
                      :documentation
                      "Stores a mapping of method names to method
objects."))
  (:documentation
   "This class serves as a superclass for local and remote server
classes. It provides storage of transport options and methods and
generic support for retrieving, adding and removing methods."))

(defmethod shared-initialize :before ((instance   server)
                                      (slot-names t)
                                      &key
                                      (transform nil transform-supplied?))
  (when transform-supplied?
    (check-type transform transform-specification)))

(defmethod server-methods ((server server))
  (hash-table-values (server-%methods server)))

(flet ((get-method (server name error?)
         (or (gethash name (server-%methods server))
             (when error?
               (error 'no-such-method :name name)))))
  (macrolet
      ((define-server-method-method (name-type &optional check?)
         `(defmethod server-method ((server server)
                                    (name   ,name-type)
                                    &key
                                    (error? t))
            ,@(when check?
                `((check-type name method-name "a legal method name")))

            (get-method server name error?))))

    (define-server-method-method string    t)
    (define-server-method-method (eql nil))))

(flet ((set-method (server name method)
         (let+ (((&structure-r/o server- (methods %methods)) server))
           ;; If SERVER already has a method named NAME, detach it cleanly
           ;; before replacing it.
           (when-let ((old (gethash name methods)))
             (detach old))

           ;; Install NEW-VALUE as new implementation of the method
           ;; named NAME.
           (setf (method-%server method) server
                 (gethash name methods)  method)))
       (remove-method1 (server name method)
         (detach (server-method server name))
         (remhash name (server-%methods server))
         method))

  (macrolet
      ((define-setf-server-method-method (action name-type new-value-type
                                          &optional check?)
         `(defmethod (setf server-method) ((new-value ,new-value-type)
                                           (server    server)
                                           (name      ,name-type)
                                           &key
                                           argument)
            (declare (ignore argument))

            ,@(when check?
                `((check-type name method-name "a legal method name")))

            ,(ecase action
               (:set    `(set-method     server name new-value))
               (:remove `(remove-method1 server name new-value))))))

    (define-setf-server-method-method :set    string    method1   t)
    (define-setf-server-method-method :set    (eql nil) method1)

    (define-setf-server-method-method :remove string    (eql nil) t)
    (define-setf-server-method-method :remove (eql nil) (eql nil))))

(defmethod detach ((server server))
  (map nil (curry #'%remove-method-with-restart-and-timeout server)
       (server-methods server)))

(defmethod print-object ((object server) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A (~D)"
            (scope-string (participant-scope object))
            (hash-table-count (server-%methods object)))))

;;; `server' creation

(defmethod make-participant-using-class
    ((class     class)
     (prototype server)
     (scope     scope)
     &rest args &key
     (transports     '())
     (converters     (default-converters))
     (introspection? (when (member (option-value '(:introspection :enabled)) '(t "1")
                                   :test #'equal)
                       t)))
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
           :converters        converters
           :transport-options transport-options
           :introspection?    introspection?
           (remove-from-plist args :transports :introspection?))))

;;; Utility functions

(defun %remove-method-with-restart-and-timeout (server method)
  "Remove METHOD from SERVER with a CONTINUE restart in place to allow
callers to ignore errors. Signal a `bordeaux-threads:timeout'
condition, if the operation does not complete within ten seconds."
  ;; Give METHOD ten seconds to detach. If one takes longer, allow
  ;; skipping it.
  (with-restart-and-timeout (10)
    (setf (server-method server (method-name method)) nil)))
