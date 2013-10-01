;;;; threaded-receiver-mixin.lisp --- A mixin for threaded receiving connectors.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.transport)

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
  (let+ (((&accessors
           (control-mutex     connector-control-mutex)
           (control-condition connector-control-condition)) connector))
    ;; Launch the thread.
    (log1 :info connector "Starting worker thread")
    (bt:make-thread (curry #'receive-messages connector)
                    :name (format nil "Worker for ~A" connector))

    ;; Wait until the thread has entered `receive-messages'.
    (bt:with-lock-held (control-mutex)
      (iter (until (connector-started? connector))
            (bt:condition-wait control-condition control-mutex)))))

(defmethod stop-receiver ((connector threaded-receiver-mixin))
  (let+ (((&accessors (thread        connector-thread)
                      (control-mutex connector-control-mutex)) connector))
    (bt:with-lock-held (control-mutex)
      (cond
        ;; If this is called from the receiver thread, there is no
        ;; need for an interruption. We can just unwind normally.
        ((eq thread (bt:current-thread))
         (log1 :info connector "In receiver thread; aborting")
         (exit-receiver))

        ((bt:thread-alive-p thread)
         ;; On very rare occasions, interrupting the thread
         ;; fails. Retry in such cases.
         (iter (while (bt:thread-alive-p thread))
               ;; Interrupt the receiver thread and abort.
               (log1 :info connector "Interrupting receiver thread with abort")
               (ignore-errors
                (bt:interrupt-thread thread #'exit-receiver))

               ;; The thread should be terminating or already have
               ;; terminated.
               (log1 :info connector "Joining receiver thread")
               (handler-case
                   (bt:with-timeout (.1)
                     (bt:join-thread thread))
                 (bt:timeout (condition)
                   (declare (ignore condition))
                   (log1 :warn connector "Interrupting receiver failed; retrying")))))))))

(defmethod receive-messages :around ((connector threaded-receiver-mixin))
  "Catch the 'terminate tag that is thrown to indicate interruption
requests."
  ;; Notify the thread which is waiting in `start-receiver'.
  (restart-case
      (progn
       (let+ (((&accessors
                (thread            connector-thread)
                (control-mutex     connector-control-mutex)
                (control-condition connector-control-condition)) connector))
         (bt:with-lock-held (control-mutex)
           (setf thread (bt:current-thread))
           (bt:condition-notify control-condition)))
       (log1 :info connector "Entering receive loop")
       (call-next-method))
    (abort (&optional condition)
      (declare (ignore condition))))
  (log1 :info connector "Left receive loop"))

(defun exit-receiver ()
  "Cause a receiver thread to exit. Has to be called from the receiver
thread."
  (ignore-errors (abort)))
