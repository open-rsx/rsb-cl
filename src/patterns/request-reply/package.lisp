;;;; package.lisp --- Package definition for the patterns.request-reply module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.request-reply
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsb
   #:rsb.patterns)

  ;; Types
  (:export
   #:method-name

   #:argument-style)

  ;; Conditions
  (:export
   #:no-such-method
   #:no-such-method-name

   #:remote-call-error
   #:remote-call-error-method
   #:remote-call-error-request

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
   #:method-name)

  ;; Remote method protocol
  (:export
   #:call)

  ;; Server Protocol
  (:export
   #:server
   #:server-methods
   #:server-method)                 ; also setf

  ;; `local-server' class
  (:export
   #:local-server)

  ;; `remove-server' class
  (:export
   #:remote-server)

  ;; Convenience and utility macros
  (:export
   #:with-methods)

  (:documentation
   "This package contains the request-reply communication pattern.

    In this communication pattern, a client submits requests to a
    servers which processes the requests and sends associated replies
    to the client."))
