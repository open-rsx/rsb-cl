;;;; error-handling-mixins.lisp --- Unit tests for the transport-related error handling mixins.
;;;;
;;;; Copyright (C) 2011-2016, 2019 Jan Moringen
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

            (def-suite* ,suite-name
              :in transport-root
              :description
              ,(format nil "Unit tests for the `~(~A~)' class." class))

            (test smoke
              `(format nil "Smoke test for the error handling ~
                            performed by the `~(~A~)' class."
                       class)

              (let ((handler (make-instance ',class)))
                ;; See
                ;; rsb.event-processing.test:error-policy-mixin-root
                ;; for an explanation of the test logic.
                (signals simple-error ,@invoke)

                (setf (processor-error-policy handler) #'continue)
                ,@invoke))))))

  (define-error-handling-mixin-tests
      error-handling-pull-receiver-mixin
      (emit ((block? t))
        (restart-case
            (error "~@<emit signaled an error.~@:>")
          (continue (&optional condition)
            (declare (ignore condition))
            nil)))

    (emit handler t))

  (define-error-handling-mixin-tests
      error-handling-push-receiver-mixin
      (receive-messages ()
        (restart-case
            (error "~@<receive-messages signaled an error.~@:>")
          (continue (&optional condition)
            (declare (ignore condition))
            nil)))

    (receive-messages handler))

  (define-error-handling-mixin-tests
      error-handling-sender-mixin
      (handle ((event event))
        (restart-case
            (error "~@<handle signaled an error.~@:>")
          (continue (&optional condition)
            (declare (ignore condition))
            nil)))

    (handle handler (make-event "/" "bla"))))
