;;;; macros.lisp --- Macros provided by the patterns.data-flow module.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

;;; Participant creation macros

(define-with-participant-macro source type)

(define-with-participant-macro sink)

;;; `with-flow-state' macro

(defun call-with-flow-state (participant desired-state if-suspended thunk)
  "Call THUNK with PARTICIPANT being in DESIRED-STATE.

   TODO"
  (let+ (((&accessors (current-state flow-state)
                      (lock          %flow-lock)
                      (condition     %flow-condition)) participant))
    (bt:with-lock-held (lock)
      (iter (until (eq current-state desired-state)) ; TODO(jmoringe, 2012-07-19): use normal error policy?
            (log:debug "~@<~A is in state ~A instead of desired state ~A~@:>"
                       participant current-state desired-state)
            (when (eq current-state :suspended)
              (restart-case ; TODO error-behavior-restart-case?
                  (funcall if-suspended (make-condition 'participant-suspended
                                                        :participant participant))
                (continue (&optional condition)
                  (declare (ignore condition))
                  (return-from call-with-flow-state nil))
                (retry ()
                  (bt:condition-wait condition lock)))))
      (log:debug "~@<~A reached desired state ~A~@:>" participant current-state)
      (funcall thunk))))

;; TODO generalize if-suspended to if-other-state?
(defmacro with-flow-state ((participant desired-state
                            &key
                            (if-suspended '#'%retry-if-suspended))
                           &body body)
  "Execute BODY with PARTICIPANT in DESIRED-STATE.

   IF-SUSPENDED has to be a function which is called if PARTICIPANT in
   state :suspended.

   While BODY is executed (if it is executed) the suspension state of
   PARTICIPANT is DESIRED-STATE and is protected against concurrent
   modification."
  `(call-with-flow-state
    ,participant ,desired-state ,if-suspended (lambda () ,@body)))

;;; Utility functions

(defun %retry-if-suspended (condition)
  (declare (ignore condition))
  (retry))
