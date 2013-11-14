;;;; processor-mixins.lisp --- Mixin classes for processor classes.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.event-processing)

;;; `error-policy-mixin' class

(defclass error-policy-mixin ()
  ((error-policy :initarg  :error-policy
                 :type     (or null function)
                 :accessor processor-error-policy
                 :initform nil
                 :documentation
                 "Stores the error policy that should be applied in
case of errors. Nil or a function to be called in case of dispatch
errors. Functions will be called with the condition object is sole
argument.
Functions installed here should be prepared to be called from multiple
threads simultaneously."))
  (:documentation
   "This class is intended to be mixed into classes that need to
handle conditions according to a client-supplied policy."))

(defmethod apply-error-policy ((processor error-policy-mixin)
                               (condition t))
  (if-let ((policy (processor-error-policy processor)))
    (funcall policy condition)
    (log:warn "~@<~A does not have a error handling policy installed; ~
               unwinding~@:>"
              processor)))

(defun call-with-error-policy (processor thunk)
  "Invoke THUNK with a handler that applies the error policy of
PROCESSOR."
  (handler-bind
      ((error (curry #'apply-error-policy processor)))
    (funcall thunk)))

(defmacro with-error-policy ((processor) &body body)
  "Execute BODY with a condition handler that applies the error policy
of processor."
  `(call-with-error-policy ,processor (lambda () ,@body)))

;;; `error-handling-dispatcher-mixin' class

(defclass error-handling-dispatcher-mixin (error-policy-mixin)
  ()
  (:documentation
   "This mixin class is intended to mixed into processor classes that
perform potentially error signaling tasks in their `dispatch'
methods. This class adds an :around method on `dispatch' that installs
restarts for error recovery and optionally calls an error policy
function."))

(defmethod dispatch :around ((processor error-handling-dispatcher-mixin)
                             (event     event))
  "Install log and ignore restarts around a call to the next
`dispatch' method. In case of an error, call the error-policy function
of PROCESSOR, if any."
  (with-error-policy (processor)
    (restart-case
        (call-next-method)
      (log (&optional condition)
        :report (lambda (stream)
                  (format stream "~@<Log a message and ignore the ~
                                  failure to dispatch event ~A.~@:>"
                          event))
        (log:warn "~@<~A failed to dispatch the event ~A~@[: ~A~]~@:>"
                  processor event condition)
        nil)
      (continue ()
        :report (lambda (stream)
                  (format stream "~@<Ignore the failure to dispatch ~
                                  event ~A.~@:>"
                          event))
        nil))))

;;; Mixin class `filtering-processor-mixin'

(defclass filtering-processor-mixin ()
  ((filter :initarg  :filter
           :type     rsb.filter:conjoin-filter
           :reader   processor-%filter
           :initform (make-instance 'rsb.filter:conjoin-filter)
           :documentation
           "The filter object is used to hold a list of filters that
act in a conjunctive manner."))
  (:documentation
   "This mixin class adds filtering of events before further
processing. Processors of this kind maintain a list of filters that
can be accessed and modified using the `processor-filters'
accessor. Events are dropped in a method on `handle' before any
further processing unless they match all filters."))

(defmethod processor-filters ((processor filtering-processor-mixin))
  (rsb.filter:filter-children (processor-%filter processor)))

(defmethod (setf processor-filters) ((new-value list)
                                     (processor filtering-processor-mixin))
  (setf (rsb.filter:filter-children (processor-%filter processor))
        new-value))

(defmethod handle ((processor filtering-processor-mixin)
                   (event     event))
  "Terminate processing of EVENT unless it matches PROCESSOR's
filters."
  (when (rsb.filter:matches? (processor-%filter processor) event)
    (call-next-method)))

;;; `deliver-timestamp-mixin' class

(defclass deliver-timestamp-mixin ()
  ()
  (:documentation
   "This class can be mixed into event processor classes that used in
event delivery contexts to attach :deliver timestamps to processed
events."))

(defmethod handle :before ((processor deliver-timestamp-mixin)
                           (event     event))
  "Attach a :deliver timestamp to EVENT."
  (setf (timestamp event :deliver) (local-time:now)))

;;; `transform-mixin' class

(defclass transform-mixin ()
  ((transform :initarg  :transform
              :reader   processor-transform
              :documentation
              "Stores a transform (in the sense of being usable with
`transform!') that should be applied to all handled data."))
  (:default-initargs
   :transform (missing-required-initarg 'transform-mixin :transform))
  (:documentation
   "This mixin class adds to processor classes the ability to apply a
transform (in the sense of being usable with `transform!') to all
handled data."))

(defmethod handle :around ((sink transform-mixin) (data t))
  (call-next-method sink (transform! (processor-transform sink) data)))
