;;; error-policy-mixin.lisp --- Unit tests for the error-policy-mixin class.
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

(in-package :rsb.event-processing.test)

(deftestsuite error-policy-mixin-root (event-processing-root)
  ((simple-processor (make-instance 'error-policy-mixin)))
  (:documentation
   "Test suite for the `error-policy-mixin' class."))

(defun signaling-function ()
  "A function that unconditionally signals an error."
  (restart-case
      (error "~@<I like to signal.~@:>")
    (log      (condition) (declare (ignore condition)) nil)
    (continue () nil)))

(macrolet
    ((define-smoke-test (name &body invoke-form)
       `(addtest (error-policy-mixin-root
		  :documentation
		  "Test basic error handling policies of the
`error-policy-mixin' class.")
	  ,name

	  ;; Error policy nil means to just unwind.
	  (setf (processor-error-policy simple-processor) nil)
	  (ensure-condition 'simple-error
	    ,@invoke-form)

	  ;; The error policy #'continue should prevent the error from
	  ;; being signaled.
	  (setf (processor-error-policy simple-processor) #'continue)
	  ,@invoke-form

	  ;; The error policy #'log-error should prevent the error
	  ;; from being signaled.
	  (setf (processor-error-policy simple-processor) #'log-error)
	  ,@invoke-form)))

  (define-smoke-test smoke/function
      (invoke-with-error-policy simple-processor #'signaling-function))

  (define-smoke-test smoke/macro
      (with-error-policy (simple-processor) (signaling-function))))
