;;;; compat.lisp --- Backwards compatibility.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Conditions

(macrolet
    ((define-comatibility-type (kind)
       (let ((predicate-name (symbolicate kind '#:-creation-error?))
             (type-name      (symbolicate kind '#:-creation-error)))
         `(progn
            (defun ,predicate-name (condition)
              (and (typep condition 'participant-creation-error)
                   (eq ,kind (participant-creation-error-kind condition))))

            (deftype ,type-name ()
              ,(format nil "This error is signaled when an attempt to
                            create a ~(~A~) fails." kind)
              '(satisfies ,predicate-name))))))

  (define-comatibility-type :reader)
  (define-comatibility-type :listener)
  (define-comatibility-type :informer))

(defun informer-creation-error-type (condition)
  (declare (ignore condition))
  t)

;;; Creation methods

;; `reader'

(define-participant-creation-uri-methods reader (scope puri:uri))

(define-participant-creation-restart-method reader (scope scope))
(define-participant-creation-restart-method reader (scope puri:uri))

;; `listener'

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))

;; `informer'

(define-participant-creation-uri-methods informer
    (scope puri:uri) (type t))

(define-participant-creation-restart-method informer
    (scope scope) (type t))
(define-participant-creation-restart-method informer
    (scope puri:uri) (type t))

;; `local-server'

(define-participant-creation-uri-methods
    rsb.patterns.request-reply:local-server (scope puri:uri))

(define-participant-creation-restart-method
    rsb.patterns.request-reply:local-server (scope scope))
(define-participant-creation-restart-method
    rsb.patterns.request-reply:local-server (scope puri:uri))

;; `remove-server'

(define-participant-creation-uri-methods
    rsb.patterns.request-reply:remote-server (scope puri:uri))

(define-participant-creation-restart-method
    rsb.patterns.request-reply:remote-server (scope scope))
(define-participant-creation-restart-method
    rsb.patterns.request-reply:remote-server (scope puri:uri))
