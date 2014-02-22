;;;; error-handling-mixins.lisp --- Unit tests for the transport-related error handling mixins.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.test)

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

              (setf (processor-error-policy simple-handler) #'continue)
              ,@invoke)))))

  (define-error-handling-mixin-tests
      error-handling-pull-receiver-mixin
      (emit ((block? t))
            (restart-case
                (error "~@<emit signaled an error.~@:>")
              (continue (&optional condition)
                (declare (ignore condition))
                nil)))

    (emit simple-handler t))

  (define-error-handling-mixin-tests
      error-handling-push-receiver-mixin
      (receive-messages ()
            (restart-case
                (error "~@<receive-messages signaled an error.~@:>")
              (continue (&optional condition)
                (declare (ignore condition))
                nil)))

    (receive-messages simple-handler))

  (define-error-handling-mixin-tests
      error-handling-sender-mixin
      (handle ((event event))
            (restart-case
                (error "~@<handle signaled an error.~@:>")
              (continue (&optional condition)
                (declare (ignore condition))
                nil)))

    (handle simple-handler (make-event "/" "bla"))))
