;;;; variables.lisp --- Global and special variables used within RSB.
;;;;
;;;; Copyright (C) 2010-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Framework scopes

(declaim (type scope *reserved-scope*))

(defvar *reserved-scope*
  (make-scope "/__rsb/" :intern? t)
  "All sub-scopes of this scope should be considered reserved for
   private implementation purposes.")

;;; Framework timestamps

(declaim (type list *framework-timestamps*))

(defvar *framework-timestamps* '(:create :send :receive :deliver)
  "Names of timestamps which are associated to certain points in the
life of RSB events.")

;;; Pseudo random state
;;;
;;; For uuid generation and such.

(declaim (type random-state *id-random-state*))

(defvar *id-random-state* (make-random-state nil)
  "Stores a `random-state' object which should be used for generation
of random IDs and similar things requiring pseudo randomness.")

;;; Configuration

(define-constant +config-file-system-placeholder+ "%system" :test #'string=)
(define-constant +config-file-user-placeholder+   "%user"   :test #'string=)
(define-constant +config-file-pwd-placeholder+    "%pwd"    :test #'string=)

(declaim (type list *default-configuration-files*))

(defvar *default-configuration-files*
  `(,+config-file-pwd-placeholder+
    ,+config-file-system-placeholder+
    ,+config-file-user-placeholder+)
  "List of configuration file names in order of decreasing priority.")

(declaim (type list *default-configuration*))

(defvar *default-configuration*
  '(((:transport :socket :enabled) . "1"))
  "A list of default configuration option values: these are used when
   no other configuration is performed.")

(declaim (type list *configuration*))

(defvar *configuration* '()
  "A list of current configuration option values.")

;;; Participant lifecycle hooks

(defvar *make-participant-hook* '()
  "Handlers registered for this hook are run when a new participant is
   created.

   The lambda-list of handlers is

     (PARTICIPANT ARGS)

   where PARTICIPANT is the new participant and ARGS are the initargs
   with which the participant instance was created.

   Handlers have to return PARTICIPANT unless PARTICIPANT should be
   replaced by some other object in which case that object has to be
   returned.")

(defvar *participant-state-change-hook* '()
  "Handlers of this hook are run when the state of a participant changes.

   The lambda-list of handlers is

     (PARTICIPANT NEW-STATE)

   where PARTICIPANT is the participant whose state changed to
   NEW-STATE.

   One NEW-STATE applicable to all kinds of participants
   is :detached.")
