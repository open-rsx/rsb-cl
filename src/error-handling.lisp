;;; error-handling.lisp --- Error handling functions used in cl-rsb.
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

(cl:in-package :rsb)


;;; Documentation references
;;

(defun documentation-ref/rsb-bug ()
  "TODO(jmoringe): document"
  (list :rsb/project "Report a bug"
	"https://code.cor-lab.org/projects/rsb/issues/new"))

(defun documentation-ref/rsb-manual (&rest parts)
  "TODO(jmoringe): document"
  (flet ((linkify (string)
	   (string-downcase (substitute #\- #\Space string))))
    (list :rsb/manual parts
	  (format nil
		  "http://docs.cor-lab.org/rsb-manual/trunk/html/~{~A.html~^#~A~}"
		  ;; "file:///home/jmoringe/code/cor-lab/rsb/rsb-manual/build/html/~{~A.html~^#~A~}"
		  (mapcar #'linkify parts)))))


;;; Useful macros
;;

(declaim (special *in-timeout?*))

(defvar *in-timeout?* nil
  "Non-nil when a timeout is active for the current thread, nil otherwise.")

(declaim (ftype (function (positive-real function) *)
		invoke-with-restart-and-timeout))

(defun invoke-with-restart-and-timeout (timeout thunk)
  "Call THUNK signaling a `bt:timeout' if it does not complete within
TIMEOUT seconds. Install a `cl:continue' restart around the
timeout/execution of THUNK which can be used to ignore errors and
timeouts.

If `*in-timeout?*' indicates that some other timeout is already active
for the current thread, install the restart but do not establish a
timeout. This is intended to prevent recursive timeouts."
  (restart-case
      ;; Give THUNK TIMEOUT seconds to complete. If it takes longer,
      ;; allow continuing via the CONTINUE restart. If another timeout
      ;; is active, call THUNK without timeout. This avoid race
      ;; conditions between timeouts and recursive timeout handling.
      (if *in-timeout?*
	  (funcall thunk)
	  (let ((*in-timeout?* t))
	    (bt:with-timeout (timeout)
	      (funcall thunk))))
    (continue ()
      :report (lambda (stream)
		(format stream "~@<Ignore the error and continue.~@:>")))))

(defmacro with-restart-and-timeout ((timeout) &body body)
  "Execute BODY signaling a `bt:timeout' if it does not complete
within TIMEOUT seconds. Install a `cl:continue' restart around the
timeout/execution of BODY which can be used to ignore errors and
timeouts.

If `*in-timeout?*' indicates that some other timeout is already active
for the current thread, install the restart but do not establish a
timeout. This is intended to prevent recursive timeouts."
  `(invoke-with-restart-and-timeout ,timeout #'(lambda () ,@body)))
