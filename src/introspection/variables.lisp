;;;; variables.lisp --- Variables used by the introspection module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

;;; Reserved scopes used by the introspection protocol
;;;
;;; Currently, there are sub-scopes for participants, hosts and
;;; processes.

(defparameter +introspection-scope+
  (make-scope (merge-scopes "/introspection" *reserved-scope*) :intern? t)
  "Scope for all introspection communication.")

;; Participants scope

(defun introspection-participants-scope (&optional
                                         (base +introspection-scope+))
  (make-scope (merge-scopes '("participants") base) :intern? t))

(defparameter +introspection-participants-scope+
  (introspection-participants-scope +introspection-scope+)
  "Default sub-scope for participant introspection.")

(defun participant-id->scope (id &optional (base +introspection-scope+))
  (merge-scopes (list (princ-to-string id))
                (introspection-participants-scope base)))

(defun scope->participant-id-or-nil (scope &optional (base +introspection-scope+))
  (when-let* ((participants-scope (introspection-participants-scope base))
              (length             (length (scope-components participants-scope)))
              (component          (nth length (scope-components scope))))
    (uuid:make-uuid-from-string component)))

(defun scope->participant-id (scope &optional (base +introspection-scope+))
  (or (scope->participant-id-or-nil scope base)
      (error "~@<No participant-id scope component in scope ~A~@:>"
             (scope-string scope))))

;; Hosts and processes scope

(defun introspection-hosts-scope (&optional
                                  (base +introspection-scope+))
  (make-scope (merge-scopes '("hosts") base) :intern? t))

(defparameter +introspection-hosts-scope+
  (introspection-hosts-scope +introspection-scope+)
  "Default sub-scope for host and process introspection.")

(defun introspection-process-scope (process-id host-id
                                    &optional
                                    (base +introspection-scope+))
  (merge-scopes (list (derive-scope-component host-id)
                      (derive-scope-component
                       (write-to-string process-id :base 10 :escape nil)))
                (introspection-hosts-scope base)))

;;; Filters used in the introspection protocol

(defvar *survey-filter*
  (rsb.filter:filter '(:or (:method :method :|request|)
                           (:method :method :|survey|))))

(defvar *broadcast-filter*
  (rsb.filter:make-filter :method :method nil))

;;; Converters used for introspection payloads

(defvar *introspection-host-converters*
  '(:fundamental-void :fundamental-utf-8-string))

(defvar *introspection-all-converters*
  `(,@*introspection-host-converters* :introspection))

;;; Local introspection database

(defvar *local-database* nil
  "Stores the introspection database for local participants.

   This singleton database object is created lazily by
   `call-with-local-database'. However, once it has been created, it
   is retained indefinitely, even across image restarts.")

(defvar *local-database-lock* (bt:make-recursive-lock "Local database lock")
  "A lock that protects accesses to `*local-database*'.")

(defun call-with-local-database (thunk &key if-does-not-exist)
  "Call THUNK with one argument, the database. Return what THUNK returns.

   During the call of THUNK the passed database object is locked.

   Depending on IF-DOES-NOT-EXIST, create the database if necessary:

   :create

     Create the database, if it does not already exist, then, in any
     case, call THUNK with the database object. Return whatever THUNK
     returns.

   nil

     If the database already exists, call THUNK with the existing
     object and return whatever THUNK returns. Otherwise do not call
     THUNK and return nil."
  (bt:with-recursive-lock-held (*local-database-lock*)
    (let+ (((&flet success (database)
              (return-from call-with-local-database
                (funcall thunk database))))
           ((&flet call (database thunk)
              (call-with-database-lock ; `with-database-lock' not yet defined
               database (lambda () (funcall thunk database))))))
      (when-let ((database *local-database*))
        (call database (lambda (database)
                         (when (introspection-participants database)
                           (funcall #'success database)))))
      (when (eq if-does-not-exist :create)
        (log:info "~@<Creating local introspection.~@:>")
        (let ((database (make-participant
                         :local-introspection +introspection-scope+)))
          (hooks:add-to-hook (participant-error-hook database)
                             (lambda (condition)
                               (log:warn "Condition~2&~2@T~A~2&~2@T~A"
                                         condition (find-restart 'continue))
                               (continue))) ; TODO hack
          (setf *local-database* database)
          (call database #'success))))))

(defmacro with-local-database ((database-var &key if-does-not-exist)
                               &body body)
  "Execute BODY with DATABASE-VAR bound to the local introspection
   database.

   For a description of IF-DOES-NOT-EXIST, see
   `call-with-local-database'."
  `(call-with-local-database
    (named-lambda with-local-database-thunk (,database-var) ,@body)
    :if-does-not-exist ,if-does-not-exist))

;;; [Un]registering participants
;;;
;;; Called to add and remote entries to/from the local introspection
;;; database as participants are created and destroyed in the local
;;; process.

(defun register-participant (participant &optional parent)
  (let ((id        (participant-id participant))
        (parent-id (when parent (participant-id parent))))
    (log:info "~@<Adding participant ~A to introspection~@:>" participant)
    (restart-case
        (with-local-database (database :if-does-not-exist :create)
          (setf (find-participant id database :parent-id parent-id)
                participant))
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Continue without introspection ~
                                  for ~A~@:>"
                          participant))
        (declare (ignore condition))))))

(defun unregister-participant (participant)
  (log:info "~@<Removing participant ~A from introspection~@:>" participant)
  (restart-case
      (with-local-database (database)
        (setf (find-participant (participant-id participant) database
                                :if-does-not-exist nil)
              nil))
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Continue without removing ~A from ~
                                introspection~@:>"
                        participant))
      (declare (ignore condition)))))

;;; Participant state change handlers

(defun exclude-from-introspection? (participant)
  (or (member (participant-kind participant)
              '(:local-introspection
                :remote-introspection :introspection-receiver)
              :test #'eq)
      (sub-scope? (participant-scope participant) +introspection-scope+)))

(defun handle-make-participant (participant args)
  ;; Register PARTICIPANT for introspection but only if it does not
  ;; look like being part of the introspection machinery itself.
  (let+ (((&plist-r/o (introspection? :introspection?) (parent :parent)) args))
    (when (and introspection? (not (exclude-from-introspection? participant)))
      (register-participant participant parent)))
  ;; Do not return a non-nil value: the hook interprets such a value
  ;; as a replacement for PARTICIPANT.
  nil)

(defgeneric handle-participant-state-change (participant new-state)
  (:method ((participant t) (new-state t))
    nil)
  (:method ((participant t) (new-state (eql :detached)))
    (unless (exclude-from-introspection? participant)
      (unregister-participant participant)))
  (:documentation
   "Handle the event of PARTICIPANT changing into state NEW-STATE.

    Most importantly, call `unregister-participant' when NEW-STATE
    is :detached.

    This generic function is not part of a public protocol. It exists
    only for local convenient and flexible dispatch."))

;;; Event handling

(hooks:add-to-hook '*make-participant-hook*
                   #'handle-make-participant) ; TODO bad for reloading

(hooks:add-to-hook '*participant-state-change-hook*
                   #'handle-participant-state-change)
