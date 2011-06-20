;;; error-handling-mixins.lisp --- Unit tests for the transport-related error handling mixins.
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
		    #'ignore-error)
	      ,@invoke

	      (setf (processor-error-policy simple-handler)
		    #'ignore-error)
	      ,@invoke)))))

  (define-error-handling-mixin-tests
      error-handling-pull-receiver-mixin
      (emit ((block? t))
	    (restart-case
		(error "~@<emit signaled an error.~@:>")
	      (log    (condition) (declare (ignore condition)) nil)
	      (ignore ()          nil)))

    (emit simple-handler t))

  (define-error-handling-mixin-tests
      error-handling-push-receiver-mixin
      (receive-messages ()
	    (restart-case
		(error "~@<receive-messages signaled an error.~@:>")
	      (log    (condition) (declare (ignore condition)) nil)
	      (ignore ()          nil)))

    (receive-messages simple-handler))

  (define-error-handling-mixin-tests
      error-handling-sender-mixin
      (handle ((event event))
	    (restart-case
		(error "~@<handle signaled an error.~@:>")
	      (log    (condition) (declare (ignore condition)) nil)
	      (ignore ()          nil)))

    (handle simple-handler (make-event "/" "bla"))))
