;;;; util.lisp --- Utility functions used in the transport.spread module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; Generic utility functions

(declaim (inline internal-real-time-in-seconds))
(defun internal-real-time-in-seconds ()
  "Return the current \"internal\" time in seconds."
  (/ (get-internal-real-time) internal-time-units-per-second))

;;; Daemon name

(defun normalize-daemon-endpoint (name host port)
  "As three values return normalized daemon name, host and port.

   Exactly one of NAME and PORT has to be non-nil."
  (let+ (((&values host port)
          (cond
            (name (network.spread:parse-daemon-name name))
            (port (values host port))
            (t    (missing-required-argument :name-or-port))))
         (port (if (stringp port)
                   (parse-integer port)
                   port))
         (name (format nil "~D~@[@~A~]" port host)))
    (values name host port)))

;;; Scope -> spread group mapping

(defvar *scope->groups-cache* nil
  "This cache maps `scope' instances to the corresponding Spread
   groups.

   The variable is special so each thread can have its own cache.")

(defvar *scope->groups-cache-max-size* 1024
  "The maximum number of allowed entries in the scope -> group mapping
   cache.")

(defun make-scope->groups-cache ()
  "Return a cache to which `*scope->groups-cache*' can be bound."
  (make-hash-table :test #'eq
                   :size *scope->groups-cache-max-size*))

(defun scope->group (scope)
  "Return a spread group name derived from SCOPE."
  (let* ((octets (sb-ext:string-to-octets (scope-string scope)))
         (hash   (ironclad:digest-sequence :md5 octets))
         (string (format nil "~(~{~2,'0x~}~)" (coerce hash 'list))))
    (setf (aref string 31) #\Null)
    string))

(defun scope->groups/no-cache (scope)
  "Return a list of spread group names derived from SCOPE and its
   super-scopes."
  (map 'list #'scope->group (super-scopes scope :include-self? t)))

(defun scope->groups (scope &optional (cache *scope->groups-cache*))
  "Like `scope->groups/no-cache', but caches results.

   The cache implementation only works efficiently, if SCOPE is
   interned."
  (or (gethash scope cache)
      (progn
        (when (>= (hash-table-count cache) *scope->groups-cache-max-size*)
          (clrhash cache))
        (setf (gethash scope cache) (scope->groups/no-cache scope)))))
