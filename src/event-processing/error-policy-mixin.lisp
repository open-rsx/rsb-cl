;;; error-policy-mixin.lisp --- A mixin for client-supplied error policies.
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

(in-package :rsb.event-processing)

(defclass error-policy-mixin ()
  ((error-policy :initarg  :error-policy
		 :type     (or null function)
		 :accessor processor-error-policy
		 :initform nil
		 :documentation
		 "Stores the error policy that should be applied in
case of errors. Nil or a function to be called in case of dispatch
errors. Functions will be called with the condition object is sole
argument.
Functions installed here should be prepared to be called from multiple
threads simultaneously."))
  (:documentation
   "This class is intended to be mixed into classes that need to
handle conditions according to a client-supplied policy."))

(defmethod apply-error-policy ((processor error-policy-mixin)
			       (condition t))
  (if-let ((policy (processor-error-policy processor)))
    (funcall policy condition)
    (log1 :warn processor "Do not have a error handling policy installed; unwinding")))

(defun invoke-with-error-policy (processor thunk)
  "Invoke THUNK with a handler that applies the error policy of
PROCESSOR."
  (handler-bind
      ((error (curry #'apply-error-policy processor)))
    (funcall thunk)))

(defmacro with-error-policy ((processor) &body body)
  "Execute BODY with a condition handler that applies the error policy
of processor."
  `(invoke-with-error-policy ,processor #'(lambda () ,@body)))
