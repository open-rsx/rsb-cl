;;;; source.lisp --- Source participant.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

(defclass source (informer
                  suspendable-mixin
                  flow-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod send :around ((informer source)
                         (event    event)
                         &key
                         (if-suspended #'%retry-if-suspended)
                         &allow-other-keys)
  "Return two values: if DATA has been sent:

     EVENT t

   or

     nil nil

   if EVENT has not been sent.

   TODO IF-SUSPENDED"
  (with-flow-state (informer :running :if-suspended if-suspended)
    (values (call-next-method) t)))

;; TODO this could be equally applicable for local data-flow
;; conditions signaled by out-direction connectors
(defmethod handle-flow-condition ((participant source)
                                  (condition   high-watermark-reached/remote))
  (log:info "~@<~A received remote high watermark reached for ~A~@:>"
            participant (which condition))
  ;; We received a remote high-watermark-reached condition. Establish
  ;; restarts for the possible reactions:
  ;;
  ;; continue
  ;;   Do nothing; PARTICIPANT continues sending.
  ;;
  ;; abort
  ;;   TODO probably stops PARTICIPANT alltogether, right?
  ;;
  ;; drop
  ;;   TODO not implemented yet, but also a possible reaction. Or add
  ;;   a :dropping state?
  ;;
  ;; suspend
  ;;   Suspend PARTICIPANT. This may be reversed when a remote
  ;;   low-watermark-reached condition is received.
  ;;
  ;; then let the handlers attached to the data-flow hook decide how
  ;; to react.
  (restart-case
      (hooks:run-hook (hooks:object-hook participant 'flow-hook) condition)
    (continue (&optional condition)
      ;; TODO report
      (declare (ignore condition))
      )

    (abort (&optional condition)
      ;; TODO report
      (declare (ignore condition))
      (log:error "not implemented")
      )

    (suspend ()                         ; TODO export symbol
      ;; TODO report
      (suspend participant))))

(defmethod handle-flow-condition ((participant source)
                                  (condition   low-watermark-reached/remote))
  (log:info "~@<~A received remote low watermark reached for ~A~@:>"
            participant (which condition))
  (restart-case
      (hooks:run-hook (hooks:object-hook participant 'flow-hook) condition)
    (resume ()
      (resume participant))))

;;; `source' creation

(defmethod make-source ((scope scope)
                        (type  t)
                        &key
                        (transports (transport-options))
                        (converters (default-converters))
                        transform)
  "Make and return a `source' instance "
  ;; Translate different kinds of errors into TODO condition
  ;; `informer-creation-failed' errors.
  (with-condition-translation
      (((error source-creation-error)
        :scope      scope
        :transports transports
        :type       type))
    (let+ (((&values source configurator)
            (make-participant 'source scope :out
                              transports converters transform
                              :type       type
                              :transports transports)))
      ;; Connect the processor of CONFIGURATOR to SOURCE as an event
      ;; handler.
      (push (rsb.ep:configurator-processor configurator)
            (rsb.ep:handlers source))

      ;; TODO this scheme should be available as a separate policy function
      ;; TODO in this scheme, there should be a timeout mechanism in case a sink reaches its high watermark and then crashes
      (let ((high-watermarks (make-hash-table :test #'equalp)))
        (hooks:add-to-hook (hooks:object-hook source 'flow-hook)
                           (lambda (condition) ; TODO make proper dispatch
                             (log:info "~@<~A received condition~@:_~2@T~A~@:_high-watermarks~@:_~2@T~A~@:>"
                                        source condition high-watermarks)
                             (typecase condition
                               (high-watermark-reached/remote
                                (incf (gethash (key condition) high-watermarks 0))
                                (when (= (hash-table-count high-watermarks) 1)
                                  (log:info "~@<First remote structure ~
                                             ~A:~A of ~A reached high ~
                                             watermark; ~
                                             suspending~@:>"
                                            (peer condition) (which condition) source)
                                  (invoke-restart 'suspend)))

                               (low-watermark-reached/remote
                                (let+ ((key   (key condition))
                                       (value (gethash key high-watermarks)))
                                  (case value
                                    ((nil)
                                     (warn "~@<Received ~A for unknown key ~A; ignoring~@:>"
                                           condition key))
                                    (1
                                     (remhash key high-watermarks))
                                    (t
                                     (setf (gethash key high-watermarks) (1- value))))
                                 (when (zerop (hash-table-count high-watermarks))
                                   (log:info "~@<Last remote structure ~
                                             ~A:~A of ~A reached low ~
                                             watermark; resuming~@:>"
                                             (peer condition) (which condition) source)
                                   (invoke-restart 'resume))))

                               (t
                                (log:info "~@<~A received not a watermark condition: ~A; forwarding to error hook~@:>"
                                          source condition)
                                (hooks:run-hook
                                 (hooks:object-hook source 'error-hook) condition))))))

      ;; Return the ready-to-use `source' instance.
      source)))

(define-participant-creation-uri-methods source
  (scope puri:uri)
  (type  t))

(define-participant-creation-restart-method source
  (scope scope)
  (type  t))

(define-participant-creation-restart-method source
  (scope puri:uri)
  (type  t))
