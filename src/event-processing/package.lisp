;;;; package.lisp --- Package definition for event-processing module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.event-processing
  (:nicknames
   #:rsb.ep)

  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:rsb)

  ;; Push source protocol
  (:export
   #:handlers)

  ;; Pull source protocol
  (:export
   #:emit)

  ;; Sink protocol
  (:export
   #:handle)

  ;; Dispatching processor protocol
  (:export
   #:dispatch)

  ;; Notification protocol
  (:export
   #:notify)

  ;; Processor class family
  (:export
   #:make-processor-class
   #:ensure-processor-class
   #:clear-processor-classes
   #:processor-classes)

  ;; Configurator protocol
  (:export
   #:make-processor
   #:collect-processor-mixins)

  ;; `configurator' class
  (:export
   #:configurator
   #:configurator-scope
   #:configurator-direction
   #:configurator-processor
   #:configurator-connectors
   #:configurator-transform)

  ;; `in-route-configurator' class
  (:export
   #:in-route-configurator
   #:configurator-filters)

  ;; `out-route-configurator' class
  (:export
   #:out-route-configurator)

  ;; `broadcast-processor' class
  (:export
   #:broadcast-processor)

  ;; `filtering-processor-mixin' class
  (:export
   #:filtering-processor-mixin
   #:processor-filters)

  ;; `deliver-timestamp-mixin' class
  (:export
   #:deliver-timestamp-mixin)

  ;; `error-policy-mixin' class
  (:export
   #:error-policy-mixin
   #:processor-error-policy
   #:apply-error-policy
   #:call-with-error-policy

   #:with-error-policy)

  ;; `error-policy-dispatcher-mixin' class
  (:export
   #:error-policy-dispatcher-mixin)

  ;; `restart-dispatcher-mixin' class
  (:export
   #:restart-dispatcher-mixin)

  ;; `error-policy-handler-mixin' class
  (:export
   #:error-policy-handler-mixin)

  ;; `restart-handler-mixin' class
  (:export
   #:restart-handler-mixin)

  ;; `transform-mixin' class
  (:export
   #:transform-mixin
   #:processor-transform)

  ;; `client' class and protocol
  (:export
   #:client
   #:client-configurator)

  ;; `scope-trie'
  (:export
   #:scope-trie
   #:make-scope-trie
   #:scope-trie-get                 ; also setf
   #:scope-trie-rem
   #:scope-trie-map
   #:scope-trie-update)

  ;; `sink-scope-trie'
  (:export
   #:sink-scope-trie
   #:make-sink-scope-trie
   #:sink-scope-trie-add
   #:sink-scope-trie-remove)

  ;; Exported for unit test
  (:export
   #:merge-implementation-infos)

  (:documentation
   "This package contains protocol, classes and methods for routing
and processing of events."))
