;;;; source.lisp --- Source participant.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

(progn ; TODO move to some other file
  (rsb.common:load-idl
   "package rsb.protocol; message HighWatermarkReached { required string which = 1; }"
   :proto
   :purpose '(:packed-size :serializer :deserializer))

  (defmethod which ((thing rsb.protocol:high-watermark-reached))
    (rsb.protocol:high-watermark-reached-which thing))

  (rsb.common:load-idl
   "package rsb.protocol; message LowWatermarkReached { required string which = 1; }"
   :proto
   :purpose '(:packed-size :serializer :deserializer))

  (defmethod which ((thing rsb.protocol:low-watermark-reached))
    (rsb.protocol:low-watermark-reached-which thing)))

(defgeneric key (condition)
  (:method-combination append)
  (:documentation
   "TODO(jmoringe): document"))

(defmethod key append ((condition watermark-condition))
  (list (which condition)))

(defmethod key append ((condition remote-flow-condition))
  (list (uuid:uuid-to-byte-array (peer condition))))

(defclass source (informer
                  suspendable-mixin
                  flow-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod send :around ((informer source)
                         (event    event)
                         &key
                         (if-suspended (lambda (condition) (declare (ignore condition)) (retry))) ; TODO don't cons up a lambda every time
                         &allow-other-keys)
  "Return two values: if DATA has been sent:

     EVENT t

   or

     nil nil

   if EVENT has not been sent.

   TODO IF-SUSPENDED"
  (with-flow-state (informer :running :if-suspended if-suspended)
    (values (call-next-method) t)))

(defmethod handle-flow-event ((participant source)
                              (event       event))
  (let+ (((&flet make-watermark-condition (class)
            (make-condition class
                            :which (which (event-data event))
                            :peer  (event-origin event))))
         ((&flet run-hooks (condition-class)
            (hooks:run-hook
             (hooks:object-hook participant 'flow-hook)
             (make-watermark-condition condition-class)))))
    (etypecase (event-data event) ; TODO dispatch properly

      (rsb.protocol:high-watermark-reached
       ;; Establish restarts for all possible reactions to the remote
       ;; high-watermark-reached condition:
       ;;
       ;; continue
       ;;   Do nothing; PARTICIPANT continues sending.
       ;;
       ;; abort
       ;;   TODO probably stops PARTICIPANT alltogether, right?
       ;;
       ;; suspend
       ;;   Suspend PARTICIPANT. This may be reversed when a remote
       ;;   low-watermark-reached condition is received.
       ;;
       ;; then let the handlers attached to the data-flow hook decide
       ;; how to react.
       (restart-case
           (run-hooks 'high-watermark-reached/remote)
         (continue (&optional condition)
           ;; TODO report
           (declare (ignore condition))
           )

         (abort (&optional condition)
           ;; TODO report
           (declare (ignore condition))
           )

         (suspend () ; TODO export symbol
           ;; TODO report
           (suspend participant))))

      (rsb.protocol:low-watermark-reached
       (restart-case
           (run-hooks 'low-watermark-reached/remote)
         (resume ()
           (resume participant)))))))

;;; `source' creation

(defmethod make-source ((scope scope)
                        (type  t)
                        &key
                        (transports (transport-options))
                        (converters (default-converters)))
  "Make and return a `source' instance "
  ;; Translate different kinds of errors into TODO condition
  ;; `informer-creation-failed' errors.
  (with-condition-translation
      (((error source-creation-failed)
        :scope      scope
        :transports transports
        :type       type))
    (let+ (((&values source configurator)
            (make-participant 'source scope :out transports converters
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
                             (log:debug "~@<~A received condition~@:_~2@T~A~@:_high-watermarks~@:_~2@T~A~@:>"
                                        source condition high-watermarks)
                             (typecase condition
                               (high-watermark-reached
                                (setf (gethash (key condition) high-watermarks) t)
                                (when (= (hash-table-count high-watermarks) 1)
                                  (log:info "~@<First peer of ~A reached high watermark; suspending~@:>"
                                            source)
                                  (invoke-restart 'suspend)))

                               (low-watermark-reached
                                (remhash (key condition) high-watermarks)
                                (when (zerop (hash-table-count high-watermarks))
                                  (log:info "~@<Last peer of ~A reached low watermark; resuming~@:>"
                                            source)
                                  (invoke-restart 'resume)))

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
