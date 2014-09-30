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
