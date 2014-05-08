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
             :type     method-name
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
                                      name)
  (when name
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

(defmacro define-lazy-creation-method (class slot transform args)
  "Define a :before method on the reader function for SLOT of CLASS
that lazily creates the slot value.

TRANSFORM has to be either :argument or :return and the corresponding
transform is installed into the created participant.

ARGS are passed to the creation function."
  (let ((method-name (symbolicate "METHOD-" slot))
        (accessor-name (symbolicate "METHOD-%" slot))
        (make-name   (symbolicate "MAKE-" slot)))
    `(defmethod ,method-name :before ((method ,class))
       ,(format nil "Lazily create the ~(~A~) when it is first requested."
                slot)
       (unless (,accessor-name method)
         (let+ (((&accessors-r/o (scope  participant-scope) ; TODO all this is not quite right yet
                                 (server method-server)) method)
                ((&structure-r/o participant- converters transform error-hook) server)
                (transform (cdr (assoc ,transform transform))))
           (setf (,accessor-name method)
                 (,make-name scope ,@args
                             :transports   (server-transport-options server)
                             :converters   converters
                             :transform    transform
                             :error-policy (lambda (condition)
                                             (hooks:run-hook
                                              error-hook condition)))))))))

;;; `server' class

(defclass server (participant)
  ((transport-options :initarg  :transport-options
                      :type     list
                      :reader   server-transport-options
                      :initform '()
                      :documentation
                      "Stores the transport options that should be
used by participants which implement the actual communication on
behalf of the server.")
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
                                      transform)
  (check-type transform transform-specification))

(defmethod server-methods ((server server))
  (hash-table-values (server-%methods server)))

(defmethod server-method ((server server)
                          (name   string)
                          &key
                          (error? t))
  (check-type name method-name "a legal method name")

  (or (gethash name (server-%methods server))
      (when error?
        (error 'no-such-method
               :name name))))

(defmethod (setf server-method) ((new-value method1)
                                 (server    server)
                                 (name      string)
                                 &key
                                 argument)
  (declare (ignore argument))

  (check-type name method-name "a legal method name")

  (let+ (((&accessors-r/o (methods server-%methods)) server))
    ;; If SERVER already has a method named NAME, detach it cleanly
    ;; before replacing it.
    (when-let ((old (gethash name methods)))
      (detach old))

    ;; Install NEW-VALUE as new implementation of the method named
    ;; NAME.
    (setf (method-%server new-value) server
          (gethash name methods)     new-value)))

(defmethod (setf server-method) ((new-value (eql nil))
                                 (server    server)
                                 (name      string)
                                 &key
                                 argument)
  (declare (ignore argument))

  (detach (server-method server name))
  (remhash name (server-%methods server))
  new-value)

(defmethod detach ((server server))
  (map nil (curry #'%remove-method-with-restart-and-timeout server)
       (server-methods server)))

(defmethod print-object ((object server) stream)
  (print-unreadable-id-object (object stream :type t)
    (format stream "~A (~D)"
            (scope-string (participant-scope object))
            (hash-table-count (server-%methods object)))))

;;; `server' creation

(defmethod make-participant-using-class ((class     class)
                                         (prototype server)
                                         (scope     scope)
                                         &rest args &key
                                         (transports (transport-options))
                                         (converters (default-converters)))
  (apply #'call-next-method class prototype scope
         :converters        converters
         :transport-options transports
         (remove-from-plist args :transports)))

;;; Utility functions

(defun %remove-method-with-restart-and-timeout (server method)
  "Remove METHOD from SERVER with a CONTINUE restart in place to allow
callers to ignore errors. Signal a `bordeaux-threads:timeout'
condition, if the operation does not complete within ten seconds."
  ;; Give METHOD ten seconds to detach. If one takes longer, allow
  ;; skipping it.
  (with-restart-and-timeout (10)
    (setf (server-method server (method-name method)) nil)))