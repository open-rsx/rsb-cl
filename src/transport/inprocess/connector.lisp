;;; connector.lisp ---
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.transport.inprocess)


;;;
;;

(defvar *by-scope* (make-hash-table :test     #'equal
				    ;:weakness :value
				    )
  "Association of scopes to event sinks interested in the respective
scopes.")

(defun by-scope (scope)
  "Return a list of connectors that are associated to SCOPE."
  (let ((key (%scope->key scope)))
    (gethash key *by-scope*)))

(defun (setf by-scope) (new-value scope)
  "Set the of handlers associated to SCOPE to NEW-VALUE."
  (let ((key (%scope->key scope)))
    (setf (gethash key *by-scope*) new-value)))


;;;
;;

(defclass connector (rsb.transport:connector)
  ()
  (:default-initargs
   :schema :inprocess
   :host   (load-time-value (hostname) t)
   :port   (load-time-value (sb-posix:getpid) t))
  (:documentation
   "DOC"))


;;; Utility functions
;;

(defun %scope->key (scope)
  "Convert the URI object URI into a scope string. "
  (scope-string scope))
