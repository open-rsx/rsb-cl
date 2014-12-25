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
   "Pushes events received on a channel to handlers (push style).

    Consists of a set of filters, a set of handlers and has a
    mechanism for dispatching matching events to these handlers."))

(register-participant-class 'listener)

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

(defun make-listener (scope-or-uri &rest args
                      &key
                      transports
                      converters
                      transform
                      error-policy
                      parent
                      introspection?
                      &allow-other-keys)
  "Listen to events on the channel designated by SCOPE-OR-URI.
   If successful return a `listener' instance. Otherwise an error of
   type `participant-creation-error' is signaled.

   TRANSPORTS determines the transport configuration that is used to
   participate in the channel. See `rsb.transport:make-connectors' for
   details regarding acceptable values of TRANSPORTS.

   CONVERTERS, if supplied, is an list that specifies a set of
   converters for particular wire-types from which the converters that
   are used in transports should be chosen. Items are of the
   form (WIRE-TYPE . CONVERTER). If CONVERTERS is not supplied, a
   default set of converters is derived from the default
   configuration.

   When non-nil, TRANSFORM is a transform object, usable with
   `rsb.event-processing:transform!', that should be applied to
   received events.

   ERROR-POLICY has to be nil or a function to be installed in the
   error hook of the created `listener'.

   If supplied, PARENT is a participant that should be considered the
   parent of the created `listener'.

   INTROSPECTION? controls whether the newly created listener
   participates in the introspection machinery. Specifically, whether
   it announces its construction and destruction and answers to
   introspection queries."
  (declare (ignore transports converters transform error-policy parent
                   introspection?))
  (apply #'make-participant :listener scope-or-uri args))
