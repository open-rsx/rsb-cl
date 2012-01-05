;;; connector.lisp --- Superclass for inprocess connector classes.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of of the GNU Lesser
;; General Public License Version 3 (the ``LGPL''), or (at your
;; option) any later version.
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
  (:metaclass connector-class)
  (:default-initargs
   :schema :inprocess
   :host   (load-time-value (hostname) t)
   :port   (load-time-value (sb-posix:getpid) t))
  (:wire-type t) ;; The Lisp process is the medium, so t (any Lisp
		 ;; object) should be a reasonable wire-type
  (:schemas   :inprocess)
  (:documentation
   "Superclass for connector classes of the inprocess transport."))


;;; Utility functions
;;

(defun %scope->key (scope)
  "Convert the URI object URI into a scope string. "
  (scope-string scope))
