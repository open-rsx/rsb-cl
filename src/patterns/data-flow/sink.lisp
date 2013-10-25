;;;; sink.lisp --- Sink participant.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

(defclass sink (listener
                flow-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

;; signaled in transport layer
;; this should be wrapped around error policy?
;; TODO merge with handle-flow-event?
(defmethod handle-condition ((participant sink) (condition high-watermark-reached))
  (let+ (((&accessors-r/o flow-informer) participant))
    (log:info "~@<~A received high watermark reached~@:>" participant)
    (restart-case
        (funcall (rsb.ep:processor-error-policy participant) condition)
      (throttle () ; TODO export name
        :report (lambda (stream)
                  (format stream "~@<Try to suspend the remote source ~
                                  until the situation has been resolved~@:>"))
        (log:info "~@<~A is trying to slow down remote source~@:>" participant)
        (send flow-informer (make-instance 'rsb.protocol:high-watermark-reached
                                           :which (string (which condition))))) ; TODO make a function?
      (drop () ; TODO export name
        :report (lambda (stream)
                  (format stream "~@<Drop messages until the ~
                                  situation has been resolved~@:>"))
        (log:info "~@<~A is dropping events TODO NOT IMPLEMENTED ~@:>" participant)))))

(defmethod handle-condition ((participant sink) (condition low-watermark-reached))
  (let+ (((&accessors-r/o flow-informer) participant))
    (log:info "~@<~A received low watermark reached~@:>" participant)
    (send flow-informer (make-instance 'rsb.protocol:low-watermark-reached
                                       :which (string (which condition)))))) ; TODO make a function?

(defmethod handle-condition ((participant sink) (condition t))
  (funcall (rsb.ep:processor-error-policy participant) condition))

;;; `sink' creation

(defmethod make-sink ((scope scope)
                      &key
                      (transports (transport-options))
                      (converters (default-converters)))
  "Make and return a `sink' instance "
  ;; Translate different kinds of errors into TODO condition
  ;; `sink-creation-failed' errors.
  (with-condition-translation
      (((error sink-creation-failed)
        :scope      scope
        :transports transports))
    (make-participant 'sink sink :in-push transports converters
                      :transports transports)))

(define-participant-creation-uri-methods sink
  (scope puri:uri))

(define-participant-creation-restart-method sink
  (scope scope))

(define-participant-creation-restart-method sink
  (scope puri:uri))
