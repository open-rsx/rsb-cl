;;;; connector-mixins.lisp --- Mixin for connector classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

;;; Mixin class `error-handling-pull-receiver-mixin'

(defclass error-handling-pull-receiver-mixin (error-policy-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into in-direction, pull-style
connector classes to provide client-supplied error handling policies
for the `emit' method."))

(defmethod emit :around ((connector error-handling-pull-receiver-mixin)
                         (block?    t))
  "Call the actual `emit' method with a condition handler that applies
   the error policy of CONNECTOR."
  (with-error-policy (connector) (call-next-method)))

;;; Mixin class `error-handling-push-receiver-mixin'

(defclass error-handling-push-receiver-mixin (error-policy-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into in-direction, push-style
connector classes to provide client-supplied error handling policies
for the `receive-messages' method."))

(defmethod receive-messages :around ((connector error-handling-push-receiver-mixin))
  "Call the actual `receive-messages' method with a condition handler
  that applies the error policy of CONNECTOR."
  (with-error-policy (connector) (call-next-method)))

;;; Mixin class `error-handling-sender-mixin'
;;;
;;; Note: almost identical to `rsb.ep:error-policy-handler-mixin' but
;;; not completely. Differences: 1) semantic difference 2) method is
;;; specialized on `event' instead of `t'.

(defclass error-handling-sender-mixin (error-policy-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into out-direction connector
    classes to provide client-supplied error handling policies for the
    `handle' method."))

(defmethod handle :around ((sink error-handling-sender-mixin)
                           (data event))
  "Call the actual `handle' method with a condition handler that
   applies the error policy of CONNECTOR."
  (with-error-policy (sink) (call-next-method)))

;;; Mixin class `restart-notification-sender-mixin'

(defclass restart-notification-sender-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into connector classes that
have to provide the usual restarts when sending notifications in a
`send-notification' method and converting the events to notifications
in a `event->notification' method."))

(defmethod send-notification :around ((connector    restart-notification-sender-mixin)
                                      (notification t))
  ;; Call the next method with `continue' restart established which
  ;; discards NOTIFICATION.
  (restart-case
      (call-next-method)
    (continue (&optional condition)
      :test   (lambda (condition)
                (typep condition '(not connection-closed)))
      :report (lambda (stream)
                (format stream "~@<Ignore the failed sending attempt ~
                                and continue with the next ~
                                notification.~@:>"))
      (declare (ignore condition)))))

(defmethod event->notification :around ((connector    restart-notification-sender-mixin)
                                        (notification t))
  ;; Call the next method with `continue' restart established that
  ;; causes NOTIFICATION to be discarded.
  (restart-case
      (call-next-method)
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Ignore the failed encoding and ~
                                continue with the next event.~@:>"))
      (declare (ignore condition)))))

;;; Mixin class `restart-notification-receiver-mixin'

(defclass restart-notification-receiver-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into connector classes that
have to provide the usual restarts when receiving notifications in a
`receive-notification' method and converting the received
notifications to events in a `notification->event' method."))

(defmethod receive-notification :around ((connector restart-notification-receiver-mixin)
                                         (block?    t))
  ;; Call the next method with `continue' restart established that
  ;; retries receiving a notification.
  (iter (restart-case
            (return-from receive-notification (call-next-method))
          (continue (&optional condition)
            :test   (lambda (condition)
                      (typep condition '(not connection-closed)))
            :report (lambda (stream)
                      (format stream "~@<Ignore the failed receiving ~
                                      attempt and continue with the ~
                                      next notification.~@:>"))
            (declare (ignore condition))))))

(defmethod notification->event :around ((connector    restart-notification-receiver-mixin)
                                        (notification t)
                                        (wire-schema  t))
  ;; Call the next method with `continue' restart established that
  ;; causes the call to return nil instead of an `event' instance.
  (restart-case
      (call-next-method)
    (continue (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Ignore the failed decoding and ~
                                continue with the next event.~@:>"))
      (declare (ignore condition)))))

;;; Mixin class `conversion-mixin'

(defclass conversion-mixin ()
  ((converter :initarg  :converter
              :accessor connector-converter
              :documentation
              "A converter to which the actual conversion work is
delegated."))
  (:default-initargs
   :converter (missing-required-initarg 'conversion-mixin :converter))
  (:documentation
   "This mixin adds methods on `domain->wire' and `wire->domain' for
the subclass which delegate the conversion tasks to a stored
converter."))

(defmethod domain->wire ((connector     conversion-mixin)
                         (domain-object t))
  "Delegate conversion of DOMAIN-OBJECT to the converter stored in
CONNECTOR."
  (domain->wire (connector-converter connector) domain-object))

(defmethod wire->domain ((connector   conversion-mixin)
                         (wire-data   t)
                         (wire-schema t))
  "Delegate the conversion of WIRE-DATA, WIRE-SCHEMA to the converter
stored in CONNECTOR."
  (wire->domain (connector-converter connector) wire-data wire-schema))

(defmethod print-object ((object conversion-mixin) stream)
  (let+ (((&structure-r/o connector- converter) object)
         (sequence? (typep converter 'sequence)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~:[~S~;(~D)~]"
              sequence? (if sequence? (length converter) converter)))))

;;; Mixin class `timestamping-receiver-mixin'

(defclass timestamping-receiver-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into connector classes that
    perform two tasks:
    1) receive notifications
    2) decode received notifications
    The associated protocol is designed to be
    direction-agnostic (i.e. should work for both push and pull)."))

(defmethod notification->event :around ((connector    timestamping-receiver-mixin)
                                        (notification t)
                                        (wire-schema  t))
  ;; Add a :receive timestamp to the generated event, if any.
  (when-let ((event (call-next-method)))
    (setf (timestamp event :receive) (local-time:now))
    event))

;;; Mixin class `timestamping-sender-mixin'

(defclass timestamping-sender-mixin ()
  ()
  (:documentation
   "This class is intended to be mixed into connector classes that
    send events."))

(defmethod event->notification :before ((connector timestamping-sender-mixin)
                                        (event     event))
  (setf (timestamp event :send) (local-time:now)))

;;; `expose-transport-metrics-mixin'

(defclass expose-transport-metrics-mixin ()
  ((expose :initarg  :expose
           :type     list
           :accessor connector-expose
           :initform '()
           :documentation
           "Controls which metrics of received notifications the
connector should expose in events constructed from these
notifications."))
  (:metaclass connector-class)
  (:options
   (:expose &slot))
  (:documentation
   "This class is intended to be mixed into connector classes that
should be able to store transport metrics of received notifications in
the events constructed from the notifications."))

(defmethod shared-initialize :after ((instance   expose-transport-metrics-mixin)
                                     (slot-names t)
                                     &key
                                     (expose '() expose-supplied?))
  (when expose-supplied?
    (setf (connector-expose instance) expose)))

(defmethod connector-expose? ((connector expose-transport-metrics-mixin)
                              (metric    symbol))
  (declare (notinline member))
  (member metric (connector-expose connector) :test #'eq))

(defmethod (setf connector-expose?) ((new-value (eql nil))
                                     (connector expose-transport-metrics-mixin)
                                     (metric    symbol))
  (removef (connector-expose connector) metric)
  new-value)

(defmethod (setf connector-expose?) ((new-value t)
                                     (connector expose-transport-metrics-mixin)
                                     (metric    symbol))
  (pushnew metric (connector-expose connector))
  new-value)

(defmethod (setf connector-expose) ((new-value symbol)
                                    (connector expose-transport-metrics-mixin))
  (setf (connector-expose connector) (list new-value))
  new-value)

;;; Mixin class `sometimes-interruptible-mixin'

(defclass sometimes-interruptible-mixin ()
  ((interruptible? :initarg  :interruptible?
                   :accessor connector-interruptible?
                   :initform (cons :interruptible nil)
                   :documentation
                   "Stores the interruptibility state of the
receiver. One of interruptible, uninterruptible and interrupting."))
  (:documentation
   "This class is intended to be mixed into threaded receiver classes
that cannot always be interrupted."))

(defmethod connector-interruptible? ((connector sometimes-interruptible-mixin))
  (car (slot-value connector 'interruptible?)))

(defmethod (setf connector-interruptible?) ((new-value symbol)
                                            (connector sometimes-interruptible-mixin))
  (let ((precondition (ecase new-value
                        (:interruptible   :uninterruptible)
                        (:uninterruptible :interruptible)
                        (:interrupting    :interruptible)))
        (cell         (slot-value connector 'interruptible?)))
    (iter (until (eq (sb-ext:compare-and-swap
                      (car cell) precondition new-value)
                     precondition))))
  new-value)

(defmethod stop-receiver :around ((connector sometimes-interruptible-mixin))
  (unwind-protect
       (progn
         (setf (connector-interruptible? connector) :interrupting)
         (call-next-method))
    (setf (car (slot-value connector 'interruptible?)) :interruptible)))

(defmethod handle :around ((connector sometimes-interruptible-mixin)
                           (event     t))
  (prog2
      (setf (connector-interruptible? connector) :uninterruptible)
      (call-next-method)
    (setf (connector-interruptible? connector) :interruptible)))

;;; Mixin class `threaded-receiver-mixin'

(defclass threaded-receiver-mixin ()
  ((thread            :type     (or null bt:thread)
                      :accessor connector-thread
                      :reader   connector-started?
                      :initform nil
                      :documentation
                      "Stores the receiver thread of the
connector. Additionally used to indicate the state of the connector,
i.e. if non-nil thread is running and did its setup stuff.")
   (control-mutex     :reader   connector-control-mutex
                      :initform (bt:make-recursive-lock
                                 "Receiver Control Mutex")
                      :documentation
                      "Required for thread startup synchronization.")
   (control-condition :reader   connector-control-condition
                      :initform (bt:make-condition-variable
                                 :name "Receiver Control Condition")
                      :documentation
                      "Required for thread startup synchronization."))
  (:documentation
   "This mixin class is intended to be mixed into message receiving
connector classes which want do so in a dedicated thread. This mixin
class takes care of managing the starting and joining of the
thread."))

(defmethod start-receiver ((connector threaded-receiver-mixin))
  (let+ (((&structure connector- started? control-mutex control-condition)
          connector))
    ;; Launch the thread.
    (log:debug "~@<~A is starting worker thread~@:>" connector)
    (bt:make-thread (curry #'receive-messages connector)
                    :name (format nil "Worker for ~A" connector))

    ;; Wait until the thread has entered `receive-messages'.
    (bt:with-lock-held (control-mutex)
      (iter (until started?)
            (bt:condition-wait control-condition control-mutex)))
    (log:debug "~@<~A started worker thread~@:>" connector)))

(defmethod stop-receiver ((connector threaded-receiver-mixin))
  (let+ (((&structure connector- thread control-mutex) connector))
    (bt:with-lock-held (control-mutex)
      (cond
        ;; If there is no thread, do nothing.
        ((not thread))

        ;; If this is called from the receiver thread, there is no
        ;; need for an interruption. We can just unwind normally.
        ((eq thread (bt:current-thread))
         (log:debug "~@<~A is in receiver thread; aborting~@:>" connector)
         (exit-receiver))

        ;; If a second thread is trying to stop the receiver thread,
        ;; try joining and fall back to interrupting.
        (t
         (iter (while (bt:thread-alive-p thread))
               ;; The thread should be terminating or already have
               ;; terminated.
               (log:debug "~@<~A is joining receiver thread~@:>" connector)
               (handler-case
                   (when
                       #-sbcl (bt:with-timeout (2) (bt:join-thread thread) t)
                       #+sbcl (not (eq (sb-thread:join-thread
                                        thread :timeout 2 :default :timeout)
                                       :timeout))
                       (return))
                 #-sbcl (bt:timeout ())
                 (error (condition)
                   (log:warn "~@<~A failed to join receiver thread ~A: ~
                              ~A Giving up~@:>"

                             connector thread condition)
                   (return)))
               (log:warn "~@<~A timed out joining receiver thread ~
                          ~A. Interrupting~@:>"
                         thread connector)

               ;; Interrupt the receiver thread and abort.
               (log:debug "~@<~A is interrupting receiver thread ~A ~
                           with abort~@:>"
                          connector thread)
               (handler-case
                   (bt:interrupt-thread thread #'exit-receiver)
                 (error (condition)
                   (log:warn "~@<~A failed to interrupt receiver ~
                              thread ~A: ~A Retrying~@:>"
                             connector thread condition)))))))
    ;; This is necessary to allow restarting the connector.
    (setf thread nil)))

(defmethod receive-messages :around ((connector threaded-receiver-mixin))
  "Catch the 'terminate tag that is thrown to indicate interruption
requests."
  ;; Notify the thread which is waiting in `start-receiver'.
  (restart-case
      (let+ (((&structure connector- thread control-mutex control-condition)
              connector))
        (bt:with-lock-held (control-mutex)
          (setf thread (bt:current-thread))
          (bt:condition-notify control-condition))
        (log:debug "~@<~A is entering receive loop~@:>" connector)
        (call-next-method))
    (abort (&optional condition)
      :report (lambda (stream)
                (format stream "~@<Abort processing for ~A~@:>" connector))
      (declare (ignore condition))))
  (log:debug "~@<~A left receive loop~@:>" connector))

(defun exit-receiver ()
  "Cause a receiver thread to exit. Has to be called from the receiver
thread."
  (ignore-errors (abort)))

;;; `threaded-message-receiver-mixin'

(defclass threaded-message-receiver-mixin (threaded-receiver-mixin)
  ()
  (:documentation
   "This mixin class combines receiving of messages and management of
a dedicated thread for receiving messages. It can therefore supply a
default implementation of the receive loop which runs in the receiver
thread."))

(defmethod receive-messages ((connector threaded-message-receiver-mixin))
  "Receive a message that can be decoded into an event. Return the
event."
  (iter (let+ (((&values notification wire-schema)
                (receive-notification connector t))
               ;; Try to convert NOTIFICATION into one or zero events
               ;; (in the latter case, EVENT is nil).
               (event (when notification
                        (notification->event connector notification wire-schema))))
              ;; Due to fragmentation of large events into multiple
              ;; notifications and error handling policies, we may not
              ;; obtain an `event' instance from the notification.
              (when event
                (dispatch connector event)))))
