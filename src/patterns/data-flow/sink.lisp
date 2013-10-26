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
(defmethod handle-flow-condition ((participant sink)
                                  (condition   high-watermark-reached/local))
  (log:info "~@<~A received local high watermark reached for ~A~@:>"
            participant (which condition))
  (restart-case
      (hooks:run-hook (participant-error-hook participant) condition)
    (throttle ()     ; TODO export name; rename to `suspend' or
                   ; `suspend/remote' or `suspend-source' or similar
                   ; for symmetry with `suspend' generic function
        :report (lambda (stream)
                  (format stream "~@<Try to suspend the remote source ~
                                  until the situation has been resolved~@:>"))
        (log:info "~@<~A is trying to slow down remote source~@:>" participant)
        (send (participant-%flow-informer participant)
              (make-instance 'high-watermark-reached/remote
                             :which (which condition)
                             :peer  (participant-id participant)))) ; TODO make a function?
      (drop () ; TODO export name
        :report (lambda (stream)
                  (format stream "~@<Drop messages until the ~
                                  situation has been resolved~@:>"))
        (log:info "~@<~A is dropping events TODO NOT IMPLEMENTED ~@:>" participant))))

(defmethod handle-flow-condition ((participant sink)
                                  (condition   low-watermark-reached/local))
  (let+ (((&accessors-r/o (flow-informer participant-%flow-informer)) participant))
    (log:info "~@<~A received local low watermark reached for ~A~@:>"
              participant (which condition))
    (send flow-informer (make-instance 'low-watermark-reached/remote
                                       :which (which condition)
                                       :peer  (participant-id participant))))) ; TODO make a function?

;;; `sink' creation

(defmethod make-sink ((scope scope)
                      &key
                      (transports (transport-options))
                      (converters (default-converters))
                      transform)
  "Make and return a `sink' instance "
  ;; Translate different kinds of errors into TODO condition
  ;; `sink-creation-failed' errors.
  (with-condition-translation
      (((error sink-creation-failed)
        :scope      scope
        :transports transports))
    (make-participant 'sink scope :in-push
                      transports converters transform
                      :transports transports)))

(define-participant-creation-uri-methods sink
  (scope puri:uri))

(define-participant-creation-restart-method sink
  (scope scope))

(define-participant-creation-restart-method sink
  (scope puri:uri))
