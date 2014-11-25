;;;; protocol.lisp --- Protocol provided by the transform module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transform)

;;; Transformation protocol

(defgeneric transform! (transform event)
  (:documentation
   "Destructively transform and return EVENT according to TRANSFORM.

    Signal an error of type `transform-error' in case something goes
    wrong. When the error is signaled, two restarts are established:

    `continue'

      Return the untransformed EVENT.

    `use-value' VALUE

      Return VALUE instead of transforming EVENT.

    When TRANSFORM is a function of one argument, call it with EVENT
    as the sole argument.

    When TRANSFORM is a sequence, all elements have to be transforms
    and the composition of these transforms is applied to EVENT. E.g.

      (transform! (list #'1+ (curry #'* 2)) 1) ≡ (1+ (* 2 1))

    ."))

;; Default behavior

(defmethod transform! :around ((transform t) (event t))
  ;; Translate `error' s to `transform-error' s and establish
  ;; `continue' and `use-value' restarts.
  (with-condition-translation
      (((error transform-error)
        :transform transform
        :object    event))
    (restart-case
        (call-next-method)
      (continue (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Continue without transforming ~
                                  ~A with ~A.~@:>"
                          event transform))
        (declare (ignore condition))
        event)
      (use-value (value)
        :report (lambda (stream)
                  (format stream "~@<Use a value instead of ~
                                  transforming ~A with ~A.~@:>"
                          event transform))
        value))))

(defmethod transform! ((transform t) (event t))
  ;; Signal an error since there is no suitable method on
  ;; `transform!'.
  (error "~@<No ~S method for ~A and ~A.~@:>"
         'transform! transform event))

(defmethod transform! ((transform function) (event t))
  (funcall transform event))

(defmethod transform! ((transform sequence) (event t))
  (reduce #'transform! transform :initial-value event :from-end t))

;;; Transform creation protocol.

(defgeneric make-transform (spec &key &allow-other-keys)
  (:documentation
   "Make and return a transform according to SPEC."))

(define-condition-translating-method make-transform ((spec symbol)
                                                     &rest args &key)
  ((error transform-creation-error)
   :spec (list* spec args)))

(defmethod make-transform ((spec symbol) &rest args &key)
  (apply #'service-provider:make-provider 'transform spec args))

(service-provider:define-service transform
  (:documentation
   "Providers of this service destructively transform events.

    Providers implement methods on the `transform!' generic function
    which accept an event, destructively modify one or more aspects of
    the event and return the modified event."))
