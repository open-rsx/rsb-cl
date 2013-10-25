;;;; mixins.lisp --- Mixins provided by the patterns.data-flow module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.data-flow)

(deftype flow-state () ; TODO move to types.lisp
  "TODO"
  '(member :suspended :running))

;;; `suspendable-mixin'

(defclass suspendable-mixin ()
  ((flow-state     :initarg  :flow-state
                   :type     flow-state
                   :accessor flow-state
                   :initform :running
                   :documentation
                   "")
   (flow-lock      :reader   %flow-lock
                   :initform (bt:make-lock "flow lock")
                   :documentation
                   "")
   (flow-condition :reader   %flow-condition
                   :initform (bt:make-condition-variable
                              :name "flow condition")
                   :documentation
                   ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod suspend ((participant suspendable-mixin))
  (let+ (((&accessors (state flow-state) (lock %flow-lock)) participant))
    (log:info "~@<~A is suspending~@:>" participant)
    (bt:with-lock-held (lock)
      (setf state :suspended))))

(defmethod resume ((participant suspendable-mixin))
  (let+ (((&accessors (state     flow-state)
                      (lock      %flow-lock)
                      (condition %flow-condition)) participant))
    (log:info "~@<~A is resuming~@:>" participant)
    (bt:with-lock-held (lock)
      (setf state :running)
      (bt:condition-notify condition))))

(defun call-with-flow-state (participant desired-state if-suspended thunk)
  "TODO"
  (let+ (((&accessors (current-state flow-state)
                      (lock          %flow-lock)
                      (condition     %flow-condition)) participant))
    (bt:with-lock-held (lock)
      (iter (until (eq current-state desired-state)) ; TODO(jmoringe, 2012-07-19): use normal error policy?
            (when (eq current-state :suspended)
              (restart-case ; TODO error-behavior-restart-case?
                  (funcall if-suspended (make-condition 'suspended
                                                        :participant participant))
                (continue (&optional condition)
                  (declare (ignore condition))
                  (return-from call-with-flow-state nil))
                (retry ()
                  (bt:condition-wait condition lock)))))
      (funcall thunk))))

;; TODO move to macros.lisp?
(defmacro with-flow-state ((participant desired-state
                            &key
                            (if-suspended '(lambda (condition)
                                            (declare (ignore condition))
                                            (retry))))
                           &body body)
  "TODO"
  `(call-with-flow-state
    ,participant ,desired-state ,if-suspended (lambda () ,@body)))

;;; `flow-mixin'

(defclass flow-mixin () ; TODO better name
  ((flow-hook     :type     list
                  :initform '()
                  :documentation
                  "Stores a list of functions to call in case of
                   flow-related events. Functions have to accept a
                   single argument, the flow event.")  ; TODO(jmoringe, 2012-07-19): unify with error-hook?
   (flow-listener :reader   flow-listener
                  :accessor %flow-listener
                  :documentation
                  "")
   (flow-informer :reader   flow-informer
                  :accessor %flow-informer
                  :documentation
                  ""))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod shared-initialize :after ((instance   flow-mixin)
                                     (slot-names t)
                                     &key
                                     scope
                                     converters
                                     transports)
  (let+ (((&accessors (informer %flow-informer)
                      (listener %flow-listener)) instance)
         (flow-scope (merge-scopes scope "/__rsb/flow")))
    ;; TODO(jmoringe, 2012-10-18): define a constant
    ;; TODO can we append the "subject" scope to avoid n:m flow-event
    ;; flooding?
    (setf informer (make-informer flow-scope t
                                  :converters converters
                                  :transports transports)
          listener (make-listener flow-scope
                                  :converters converters
                                  :transports transports)) ; TODO(jmoringe, 2012-07-26): forward error hooks
    (push (curry #'handle-flow-event instance) (rsb.ep:handlers listener))))

(defmethod detach :before ((participant flow-mixin)) ; TODO why :before instead of call-next-method?
  (let+ (((&flet detach-one (participant)
            (with-restart-and-timeout (10)
              (detach participant)))))
    (detach-one (flow-listener participant))
    (detach-one (flow-informer participant))))
