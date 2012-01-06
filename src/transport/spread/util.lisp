;;; util.lisp --- Utility functions used in the spread backend.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the GNU Lesser General
;; Public License Version 3 (the ``LGPL''), or (at your option) any
;; later version.
;;
;; Software distributed under the License is distributed on an ``AS
;; IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the LGPL for the specific language governing rights
;; and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html or
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.transport.spread)


;;; Generic utility functions
;;

(defun internal-real-time-in-seconds ()
  "Return the current \"internal\" time in seconds."
  (/ (get-internal-real-time)
     internal-time-units-per-second))


;;; Scope -> spread group mapping
;;

(declaim (special *scope->groups-cache* *scope->group-cache-max-size*))

(defvar *scope->groups-cache* (make-hash-table :test #'eq
					       :size 1024)
  "This cache maps `scope' instances to the corresponding spread
groups. The variable is special so each thread can have its own
cache.")

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

(defun scope->groups (scope)
  "Like `scope->groups/no-cache', but caches results.
The cache implementation only works efficiently, if SCOPE is
interned."
  (or (gethash scope *scope->groups-cache*)
      (progn
	(when (>= (hash-table-count *scope->groups-cache*)
		  *scope->groups-cache-max-size*)
	  (clrhash *scope->groups-cache*))
	(setf (gethash scope *scope->groups-cache*)
	      (scope->groups/no-cache scope)))))
