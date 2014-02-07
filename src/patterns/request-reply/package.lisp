;;;; package.lisp --- Package definition for the patterns.request-reply module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.request-reply
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsb)

  ;; Types
  (:export
   #:method-name

   #:argument-style)

  ;; Conditions
  (:export
   #:no-such-method
   #:no-such-method-name

   #:remote-call-failed
   #:remote-call-failed-method
   #:remote-call-failed-request

   #:remote-method-execution-error)

  ;; Future protocol
  (:export
   #:future-done?
   #:future-result
   #:future-error)

  ;; `future' class
  (:export
   #:future)

  ;; Method protocol
  (:export
   #:method-server
   #:method-name)

  ;; Remote method protocol
  (:export
   #:call)

  ;; Server Protocol
  (:export
   #:server
   #:server-methods
   #:server-method)

  ;; `local-server' class
  (:export
   #:local-server
   #:make-local-server)

  ;; `remove-server' class
  (:export
   #:remote-server
   #:make-remote-server)

  ;; Convenience and utility macros
  (:export
   #:with-local-server
   #:with-methods

   #:with-remote-server)

  (:documentation
   "This package contains the request-reply communication pattern.

    In this communication pattern, a client submits requests to a
    servers which processes the requests and sends associated replies
    to the client."))
