;;;; variables.lisp --- Global and special variables used within RSB.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013, 2014 Jan Moringen
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

(declaim (special *framework-timestamps*))

(defvar *framework-timestamps* '(:create :send :receive :deliver)
  "Names of timestamps which are associated to certain points in the
life of RSB events.")

;;; Pseudo random state
;;;
;;; For uuid generation and such.

(declaim (special *id-random-state*))

(defvar *id-random-state* (make-random-state nil)
  "Stores a `random-state' object which should be used for generation
of random IDs and similar things requiring pseudo randomness.")

;;; Configuration

(declaim (special *default-configuration-files*))

(defvar *default-configuration-files*
  `(;; Current directory
    "rsb.conf"

    ;; User-specific configuration
    #+(and unix (not darwin)) #P"~/.config/rsb.conf"
    ;;#+darwin                ?
    #+windows                 ,(merge-pathnames
                                "rsb.conf" (user-homedir-pathname))

    ;; System-wide configuration
    #+(and unix (not darwin)) #P"/etc/rsb.conf"
    ;;#+darwin                ?
    #+windows                 #P"/rsb.conf"
    )
  "List of configuration file names in order of decreasing priority.")

(declaim (special *default-configuration*))

(defvar *default-configuration* nil
  "DOC")
