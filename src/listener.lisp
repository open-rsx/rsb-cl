;;;; listener.lisp --- Listeners receive events that are broadcast on a bus.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass listener (receiving-client)
  ((direction :allocation :class
              :initform :in-push)
   (handlers  :initarg  :handlers
              :type     list
              :accessor rsb.ep:handlers
              :initform '()
              :documentation
              "Stores the list of handlers two which events received
               by this listener should be dispatched."))
  (:documentation
   "A listener consists of a set of filters, a set of handlers and has
a mechanism for dispatching matching events to these handlers."))

(defmethod (setf rsb.ep:handlers) :around ((new-value list)
                                           (listener  listener))
  (let* ((configurator (rsb.ep:client-configurator listener))
         (old-value    (rsb.ep:handlers listener))
         (added        (set-difference new-value old-value))
         (removed      (set-difference old-value new-value)))
    (prog1
        (call-next-method)
      (log:debug "~@<~A added handlers ~{~A~^, ~}~@:>" listener added)
      (log:debug "~@<~A removed handlers ~{~A~^, ~}~@:>" listener removed)
      (iter (for handler in added)
            (rsb.ep:notify configurator handler :handler-added))
      (iter (for handler in removed)
            (rsb.ep:notify configurator handler :handler-removed)))))

;;; `listener' creation

(defmethod make-listener ((scope scope)
                          &rest args &key
                          (transports (transport-options))
                          (converters (default-converters))
                          transform
                          error-policy)
  (declare (ignore transform error-policy))
  ;; Translate different kinds of errors into
  ;; `listener-creation-error' errors.
  (with-condition-translation
      (((error listener-creation-error)
        :kind       :listener
        :scope      scope
        :transports transports))
    (apply #'make-participant 'listener scope
           :transports transports
           :converters converters
           args)))

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))
