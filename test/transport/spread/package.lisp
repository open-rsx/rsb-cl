;;;; package.lisp --- Package definition for unit tests of the transport.spread module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.transport.spread.test
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions
   #:lift

   #:nibbles

   #:rsb
   #:rsb.transport
   #:rsb.transport.spread

   #:rsb.test
   #:rsb.transport.test)

  (:import-from #:rsb.transport.spread
   #:assembly-complete?
   #:assembly-concatenated-data

   #:assembly-pool-count
   #:merge-fragment

   #:assembly-pool
   #:pruning-assembly-pool

   #:event->notifications

   #:*scope->groups-cache*
   #:*scope->groups-cache-max-size*
   #:scope->group
   #:scope->groups/no-cache
   #:scope->groups

   #:connection

   #:in-connector
   #:message->event

   #:in-pull-connector

   #:out-connector)

  (:documentation
   "This package contains unit tests for the transport.spread
module"))

(cl:in-package #:rsb.transport.spread.test)

(deftestsuite transport-spread-root (transport-root)
  ((spread-port (asdf:component-property
                 (asdf:find-system :cl-rsb-test) :spread-port))
   common-args)
  (:setup
   (setf common-args `(:port      ,spread-port
                       :converter :fundamental-null)))
  (:documentation
   "Root unit test suite for the transport.spread module."))
