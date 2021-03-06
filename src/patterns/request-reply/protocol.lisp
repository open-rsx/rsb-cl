;;;; protocol.lisp --- Protocols used in the pattern.request-reply module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; Future protocol

(defgeneric future-done? (future)
  (:documentation
   "Return one of :DONE and :FAILED if the operation the result of
    which is represented by FUTURE did finish and nil otherwise."))

(defgeneric future-result (future
                           &key
                           timeout
                           error?)
  (:documentation
   "Obtain and return the result represented by FUTURE. If the
    operation producing the result is not done, block until it
    finishes or the specified waiting time is exceeded. Return two
    values: the result of the operation and one of :DONE, :FAILED
    and :TIMEOUT indicating how the associated operation finished. The
    values :FAILED and :TIMEOUT can only be returned if ERROR? is
    nil (see below).

    TIMEOUT specifies the maximum amount of time in seconds to wait
    for the operation to finish. If timeout is nil, wait indefinitely.

    If ERROR? is non-nil (the default), signal an error or a condition
    of type `bordeaux-threads:timeout' if the operation producing the
    result fails or a timeout occurs respectively."))

(defgeneric (setf future-result) (new-value future)
  (:documentation
   "Set NEW-VALUE as the result of the operation associated to
    FUTURE. The NEW-VALUE can be retrieved from FUTURE using
    `future-result' afterward. Threads waiting for the result in
    `future-result' will be woken up."))

(defgeneric (setf future-error) (new-value future)
  (:documentation
   "Indicate that the operation associated to FUTURE failed and set
    NEW-VALUE as the error condition. Afterward, calling
    `future-result' may signal the error NEW-VALUE. NEW-VALUE can be a
    condition object, a string or a list of the
    form (CONDITION-CLASS-NAME KW1 VALUE1 ...). All three forms have
    to be usable as arguments to `error'."))

;;; Method protocol

(defgeneric method-name (method)
  (:documentation
   "Return the name of METHOD."))

;;; Local and remote method invocation protocol

(defgeneric call (server method request
                  &key
                  block?
                  timeout
                  return)
  (:argument-precedence-order request method server)
  (:documentation
   "Call METHOD of SERVER with argument REQUEST and return the result
    of the method call. If a remote call fails for some reason, an
    error of type `remote-call-error' is signaled.

    If BLOCK? is non-nil, the call blocks until a result is available
    or an error occurs. Otherwise, an object implementing the future
    protocol is returned.

    If BLOCK? is non-nil, the value of TIMEOUT, when non-nil, controls
    the amount of time to wait for a reply before the blocking call
    signals a timeout error. If TIMEOUT is not supplied or nil,
    blocking calls wait indefinitely.

    RETURN controls which kind of result is (ultimately,
    i.e. potentially after forcing the returned future) returned by
    the method call. Valid values are :event, which causes the whole
    reply event to be returned, and :payload, which causes the payload
    of the reply event to be returned."))

;;; Server protocol

(defgeneric server-methods (server)
  (:documentation
   "Return a list of methods provided by SERVER."))

(defgeneric server-method (server name &key error?)
  (:documentation
   "Return the method named NAME of SERVER.

    Signal a `type-error' if NAME is not a valid method name for
    SERVER.

    If no such method exists and ERROR? is non-nil signal a
    `no-such-method' error. Otherwise return nil."))

(defgeneric (setf server-method) (new-value server name
                                  &key argument)
  (:argument-precedence-order server name new-value)
  (:documentation
   "Store NEW-VALUE as the method named NAME of SERVER. NEW-VALUE can
    be a method instance a thing like a function based on which a
    method can be made.

    If NEW-VALUE is nil, the method stored for NAME is removed from
    SERVER.

    When supplied, ARGUMENT has to be either :event or :payload
    causing the associated callback function of NEW-VALUE to receive
    the request event or just its payload respectively.

    Signal a `type-error' if NAME is not a valid method name for
    SERVER."))
