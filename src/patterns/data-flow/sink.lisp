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
(defmethod handle-condition ((participant sink) (condition condition))
  "TODO(jmoringe): document"
  (let+ (((&accessors-r/o flow-informer) participant))
   (typecase condition

     (high-watermark-reached
      (log:info "~@<~A received high watermark reached~@:>" participant)
      (restart-case
          (funcall (rsb.ep:processor-error-policy participant) condition)
        (throttle ()
          :report (lambda (stream)
                    (format stream "~@<Try to suspend the remote source ~
                                    until the situation has been resolved.~@:>"))
          (log:info "~@<~A is trying to slow down remote source~@:>" participant)
          (send flow-informer (make-instance 'rsb.protocol:high-watermark-reached
                                             :which (string (which condition)))))
        (drop ()
          :report (lambda (stream)
                    (format stream "~@<Drop messages until the ~
                                    situation has been resolved.~@:>"))
          (log:info "~@<~A is dropping events TODO NOT IMPLEMENTED ~@:>" participant))))

     (low-watermark-reached
      (log:info "~@<~A received low watermark reached~@:>" participant)
      (send flow-informer (make-instance 'rsb.protocol:low-watermark-reached
                                         :which (string (which condition)))))

     (t
      (funcall (rsb.ep:processor-error-policy participant) condition)))))
