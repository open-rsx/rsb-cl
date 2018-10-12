;;;; sender-receiver.lisp --- Protocol-aware connection for the Spread transport.
;;;;
;;;; Copyright (C) 2016-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport.spread)

;;; `connection-user-mixin'

(defclass connection-user-mixin ()
  ((connection :initarg  :connection
               :reader   connection))
  (:default-initargs
   :connection (missing-required-initarg 'connection-user-mixin :connection)))

;;; `message-receiver'

(defclass message-receiver (connection-user-mixin
                            threaded-message-receiver-mixin
                            restart-notification-receiver-mixin
                            error-handling-push-receiver-mixin
                            rsb.ep:broadcast-processor)
  ((assembly-pool :initarg  :assembly-pool
                  :type     assembly-pool
                  :reader   assembly-pool
                  :writer   (setf %assembly-pool)
                  :documentation
                  "Stores the `assembly-pool' instances used for
                   assembling notifications.")
   (state         :type     (member :active :shutdown :abort)
                  :accessor %state
                  :initform :active
                  :documentation
                  "Stores the current state of the receiver.

                   Only used during shutdown."))
  (:documentation
   "Asynchronously receives and assembles notifications."))

(defmethod initialize-instance :after ((instance message-receiver)
                                       &key
                                       assembly-pool
                                       age-limit)
  (unless assembly-pool
    (setf (%assembly-pool instance)
          (if age-limit
              (make-instance 'pruning-assembly-pool
                             :age-limit age-limit)
              (make-instance 'assembly-pool)))))

(defmethod notify ((recipient message-receiver)
                   (subject   (eql t))
                   (action    (eql :attached)))
  (start-receiver recipient))

(defmethod notify ((recipient message-receiver)
                   (subject   (eql t))
                   (action    (eql :detaching)))
  (loop :while (eq (%state recipient) :active)
        :do (sb-ext:cas (slot-value recipient 'state) :active :shutdown)))

(defmethod notify ((recipient message-receiver)
                   (subject   (eql t))
                   (action    (eql :detached)))
  (stop-receiver recipient)
  (detach (assembly-pool recipient)))

(defmethod notify ((recipient message-receiver)
                   (subject   scope)
                   (action    (eql :attached)))
  (let+ (((&values ref-count group-count promise)
          (ref-group (connection recipient) (scope->group subject)
                     :waitable? t)))
    ;; Wait for the Spread group joining operation to complete.
    (values (lparallel:force promise)
            (and (= ref-count 1) (= group-count 1)))))

(defmethod notify ((recipient message-receiver)
                   (subject   scope)
                   (action    (eql :detached)))
  (let+ (((&values &ign group-count promise)
          (unref-group (connection recipient) (scope->group subject)
                       :waitable? t)))
    ;; Wait for the Spread group leaving operation to complete.
    (when (eq (%state recipient) :abort)
      (log:warn "~@<Receiver thread aborted, not waiting for ~
                 confirmation for group leaving operation for ~
                 ~A.~@:>"
                subject)
      (return-from notify))
    (values (lparallel:force promise) (zerop group-count))))

(defmethod receive-messages :around ((connector message-receiver))
  (unwind-protect
       (call-next-method)
    (loop :while (eq (%state connector) :active)
          :do (sb-ext:cas (slot-value connector 'state) :active :abort))))

(defmethod receive-notification ((connector message-receiver) (block? t))
  ;; Delegate receiving a notification to the connection of CONNECTOR.
  (receive-message (connection connector) block?))

(defmethod notification->event ((connector    message-receiver)
                                (notification wire-notification)
                                (wire-schema  t))
  (let+ (((&accessors-r/o assembly-pool) connector)
         ((&structure-r/o wire-notification- buffer end) notification))
    ;; Try to unpack NOTIFICATION into a `fragmented-notification'
    ;; instance. Signal `decoding-error' if that fails.
    ;;
    ;; After unpacking, there are two possible cases:
    ;; 1. NOTIFICATION (maybe in conjunction with previously received
    ;;    notifications) forms a complete event
    ;; 2. NOTIFICATION does not form a complete event. In this case,
    ;;    return `nil'.
    (with-condition-translation
        (((error decoding-error)
          :encoded          buffer
          :format-control   "~@<The data could not be unpacked as a ~
                             protocol buffer of kind ~S.~:@>"
          :format-arguments '(fragmented-notification)))
      (assemble-notification
       assembly-pool (pb:unpack buffer
                                (make-instance 'fragmented-notification)
                                0 end)))))

;;; Error handling

(macrolet
    ((define-apply-error-policy-method (condition)
       `(defmethod apply-error-policy ((processor message-receiver)
                                       (condition ,condition))
          (if (eq (%state processor) :shutdown)
              (progn
                (log:debug "~@<~A got ~A in state ~A; exiting receiver~@:>"
                           processor condition (%state processor))
                (exit-receiver))
              (call-next-method)))))

  (define-apply-error-policy-method connection-unexpectedly-closed)
  (define-apply-error-policy-method network.spread:spread-error))

;;; `message-sender'

(defclass message-sender (connection-user-mixin)
  ()
  (:documentation
   "Sends serializes, fragments and sends notifications."))

(defmethod handle ((sink message-sender) (data outgoing-notification))
  (let ((notifications (event->notification sink data)))
    (declare (type function notifications))
    (loop :for notification = (funcall notifications)
          :while notification
          :do (send-notification sink notification))))

(defmethod event->notification ((connector message-sender)
                                (event     outgoing-notification))
  (let+ ((max-fragment-size 100000) ; TODO get the proper value
         ((&structure-r/o outgoing-notification- destination notification wire-data)
          event)
         (splitter (split-notification notification wire-data max-fragment-size)))
    (lambda ()
      (let+ (((&values fragment part num-parts) (funcall splitter)))
        (when fragment
          (let* ((buffer       (pb:pack* fragment))
                 (notification (make-destined-wire-notification
                                destination buffer (length buffer))))
            (declare (type simple-octet-vector buffer))
            (values notification part num-parts)))))))

(defmethod send-notification ((connector    message-sender)
                              (notification destined-wire-notification))
  ;; Send NOTIFICATION using `send-message'. The primary purpose of
  ;; this method is sending the notifications with restarts installed.
  (let+ (((&accessors-r/o connection) connector)
         ((&structure-r/o destined-wire-notification- destination) notification))
    (send-message connection destination notification)))
