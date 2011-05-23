;;; util.lisp --- Utility functions used in the spread backend.
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

(in-package :rsb.transport.spread)


;;; Generic utility functions
;;

(defun internal-real-time-in-seconds ()
  "Return the current \"internal\" time in seconds."
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defun parse-spread-name (name)
  "Split NAME into a port and host components.
Return two values the host component as string or nil if it not
specified and the port component as string."
  (let ((@ (position #\@ name)))
    (if @
        (values (subseq name (1+ @)) (subseq name 0 @))
	(values nil name))))


;;; Scope -> spread group mapping
;;

(defvar *scope-group-cache* (make-hash-table :test #'eq)
  "This cache maps `scope' instances to the corresponding spread
groups.")

(defvar *scope-group-cache-max-size* 1024
  "The maximum number of allowed entries in the scope -> group mapping
cache.")

(defun scope->group (scope)
  "Return a spread group name derived from the SCOPE.
This function uses a caching implementation which only works
efficiently, if SCOPE is interned."
  (or (gethash scope *scope-group-cache*)
      (progn
	(when (> (hash-table-count *scope-group-cache*)
		 *scope-group-cache-max-size*)
	  (clrhash *scope-group-cache*))
	(setf (gethash scope *scope-group-cache*)
	      (scope->group/no-cache scope)))))

(defun scope->group/no-cache (scope)
  "Return a spread group name derived from SCOPE."
  (let* ((octets (sb-ext:string-to-octets (rsb:scope-string scope)))
	 (hash   (ironclad:digest-sequence :md5 octets))
	 (string (format nil "~(~{~2,'0x~}~)" (coerce hash 'list))))
    (setf (aref string 31) #\Null)
    string))

(defun scope->groups (scope)
  "Return a list of spread group names derived from SCOPE and its
super-scopes."
  (map 'list #'scope->group (super-scopes scope :include-self? t)))
