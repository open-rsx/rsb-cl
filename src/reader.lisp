;;;; reader.lisp --- Pull-based receiving participant class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defclass reader (receiving-client)
  ((direction :allocation :class
              :initform :in-pull))
  (:documentation
   "Called by client to receive events on a channel (pull style).

    The client makes blocking or non-blocking calls to `receive' to
    receive the next event."))

(register-participant-class 'reader)

(defmethod receive ((reader reader)
                    &key
                    (block? t))
  (let ((processor (rsb.ep:configurator-processor
                    (rsb.ep:client-configurator reader))))
    (rsb.ep:emit processor block?)))

;;; `reader' creation

(defun make-reader (scope-or-uri &rest args
                    &key
                    transports
                    converters
                    transform
                    error-policy
                    parent
                    introspection?
                    &allow-other-keys)
  "Receive events on the channel designated by SCOPE-OR-URI.
   If successful, return a `reader' instance. Otherwise an error of
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
   error hook of the created `reader'.

   If supplied, PARENT is a participant that should be considered the
   parent of the created `reader'.

   INTROSPECTION? controls whether the newly created reader
   participates in the introspection machinery. Specifically, whether
   it announces its creation and destruction and answers to
   introspection queries.

   The resulting `reader' instance can be used to receive data in
   \"pull\" manner using the `receive' function."
  (declare (ignore transports converters transform error-policy parent
                   introspection?))
  (apply #'make-participant :reader scope-or-uri args))
