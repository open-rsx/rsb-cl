;;; error-handling-mixins.lisp --- Unit tests for the transport-related error handling mixins.
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

(in-package :rsb.transport.test)

(macrolet
    ((define-error-handling-mixin-tests (class
					 (method-name (&rest method-args) &body method-body)
					 &body invoke)
       (let ((suite-name (symbolicate class "-ROOT")))
	 `(progn
	    (defmethod ,method-name ((connector ,class) ,@method-args)
	      ,@method-body)

	    (deftestsuite ,suite-name (transport-root)
	      ((simple-handler (make-instance ',class)))
	      (:documentation
	       ,(format nil "Unit tests for the `~(~A~)' class."
			class)))

	    (addtest (,suite-name
		      :documentation
		      `(format nil "Smoke test for the error handling ~
performed by the `~(~A~)' class."
			       class))
	      smoke

	      ;; See rsb.event-processing.test:error-policy-mixin-root
	      ;; for an explanation of the test logic.
	      (ensure-condition 'simple-error
		,@invoke)

	      (setf (processor-error-policy simple-handler)
		    #'continue)
	      ,@invoke

	      (setf (processor-error-policy simple-handler)
		    #'log-error)
	      ,@invoke)))))

  (define-error-handling-mixin-tests
      error-handling-pull-receiver-mixin
      (emit ((block? t))
	    (restart-case
		(error "~@<emit signaled an error.~@:>")
	      (log      (condition) (declare (ignore condition)) nil)
	      (continue ()          nil)))

    (emit simple-handler t))

  (define-error-handling-mixin-tests
      error-handling-push-receiver-mixin
      (receive-messages ()
	    (restart-case
		(error "~@<receive-messages signaled an error.~@:>")
	      (log      (condition) (declare (ignore condition)) nil)
	      (continue ()          nil)))

    (receive-messages simple-handler))

  (define-error-handling-mixin-tests
      error-handling-sender-mixin
      (handle ((event event))
	    (restart-case
		(error "~@<handle signaled an error.~@:>")
	      (log      (condition) (declare (ignore condition)) nil)
	      (continue ()          nil)))

    (handle simple-handler (make-event "/" "bla"))))
