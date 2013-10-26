;;;; protocol.lisp --- Protocols used in the patterns.data-flow module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

;;; Flow-condition protocol

(defgeneric key (condition)
  (:method-combination append)
  (:documentation
   "TODO(jmoringe): document"))

;;; Flow-event handling protocol

(defgeneric handle-flow-condition (participant condition) ; TODO merge with some other generic function?
  (:documentation
   "TODO"))

(defmethod handle-flow-condition ((participant t) (condition event))
  (handle-flow-condition participant (event-data condition)))

(defmethod handle-flow-condition ((participant t) (condition t))
  (hooks:run-hook (rsb:participant-error-hook participant) condition))

;;; Source protocol
;;;
;;; Note that "source" refers to a conceptional graph of
;;; participants. In such a graph, a source sends events to other
;;; participants, in particular "sinks".
;;;
;;; A source is a suspendable participant which receives data-flow
;;; events.

(defgeneric suspend (participant)
  (:documentation
   "Temporarily suspend PARTICIPANT, preventing it from sending
    events.

    The effect can be reversed by calling `resume' on PARTICIPANT."))

(defgeneric resume (participant)
  (:documentation
   "Resume a the suspended PARTICIPANT, allowing it to send events.

    Only legal after `suspend' has been called PARTICIPANT."))

(defgeneric make-source (scope-or-uri type
                         &key
                         transports
                         converters
                         transform)
  (:documentation
   "TODO(jmoringe): document

    TRANSPORTS determines the transport configuration that should be
    used to make the provided methods available to other
    participants. See `rsb.transport:make-connectors' for details
    regarding acceptable values of TRANSPORTS.

    CONVERTERS, if supplied, is an list that specifies a set of
    converters for particular wire-types from which the converters
    that are used in transports should be chosen. Items are of the
    form (WIRE-TYPE . CONVERTER). If CONVERTERS is not supplied, a
    default set of converters is derived from the default
    configuration.

    If the source cannot be created, an error of type
    `source-creation-failed' is signaled."))

;;; Sink protocol
;;;
;;; Note that "sink" refers to a conceptional graph of
;;; participants. In such a graph, a sink receives events from other
;;; participants, in particular "sources".

;; TODO there should be a client-level protocol for communicating
;; watermark information like the follow.
;;
;; TODO Check how COOKIE and WATERMARK fit in with the watermark
;; condition classes. maybe
;; note-watermark-condition (participant condition)?
;; or event just use `handle-flow-event' with local (as opposed to
;; remote) conditions?
(defgeneric note-watermark-reached (participant cookie watermark)
  (:documentation
   "Inform PARTICIPANT that WATERMARK associated to COOKIE has been
    reached.

    COOKIE identifies the structure in which the watermark has been
    reached.

    WATERMARK has to be :low or :high."))

(defgeneric make-sink (scope-or-uri
                       &key
                       transports
                       converters
                       transform)
  (:documentation
   "TODO(jmoringe): document

    TRANSPORTS determines the transport configuration that should be
    used to make the provided methods available to other
    participants. See `rsb.transport:make-connectors' for details
    regarding acceptable values of TRANSPORTS.

    CONVERTERS, if supplied, is an list that specifies a set of
    converters for particular wire-types from which the converters
    that are used in transports should be chosen. Items are of the
    form (WIRE-TYPE . CONVERTER). If CONVERTERS is not supplied, a
    default set of converters is derived from the default
    configuration.

    If the sink cannot be created, an error of type
    `sink-creation-failed' is signaled."))
