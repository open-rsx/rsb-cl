;;;; processor-mixins.lisp --- Mixin classes for processor classes.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
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
                 "Stores the policy that should be applied in case of
                  errors.

                  Nil or a function to be called in case of dispatch
                  errors. Functions will be called with the condition
                  object is sole argument.  Functions installed here
                  should be prepared to be called from multiple
                  threads simultaneously."))
  (:documentation
   "Intended to be mixed into classes that need to handle conditions
    according to a client-supplied policy."))

(defmethod apply-error-policy ((processor error-policy-mixin)
                               (condition t))
  (if-let ((policy (processor-error-policy processor)))
    (funcall policy condition)
    (log:warn "~@<~A does not have a error handling policy installed; ~
               unwinding~@:>"
              processor)))

(defmacro with-error-policy ((processor) &body body)
  "Execute BODY with a condition handler that applies the error policy
   of processor."
  `(flet ((with-error-policy-thunk () ,@body))
     (declare (dynamic-extent #'with-error-policy-thunk))
     (call-with-error-policy ,processor #'with-error-policy-thunk)))

(defun call-with-error-policy (processor thunk)
  "Call THUNK with a handler that applies PROCESSOR's error policy.

   While PROCESSOR's error policy is applied, an equivalent handler is
   established allow error during application of the error policy to
   be handle by the error policy."
  (declare (type function thunk))
  (flet ((apply-policy (condition)
           (with-error-policy (processor)
             (apply-error-policy processor condition))))
    (declare (dynamic-extent #'apply-policy))
    (handler-bind ((error #'apply-policy)) (funcall thunk))))

;; Mixin classes `{error-policy,restart}-{dispatcher,handler}-mixin'

(macrolet
    ((define-error-policy+restart-mixins (method name)
       (let ((error-policy-class-name (symbolicate
                                       '#:error-policy- name '#:-mixin))
             (restart-class-name      (symbolicate
                                       '#:restart- name '#:-mixin)))
         `(progn

            (defclass ,error-policy-class-name ()
              ()
              (:documentation
               ,(format nil "This mixin class is intended to be mixed ~
                             into processor classes that perform ~
                             potentially error signaling tasks in ~
                             their `~(~A~)' methods. This class adds ~
                             an :around method on ~:*`~(~A~)' that ~
                             installs restarts for error recovery and ~
                             optionally calls an error policy ~
                             function."
                        name)))

            (defmethod ,method :around ((processor ,error-policy-class-name)
                                        (data      t))
              ;; In case of an error, call the error-policy
              ;; function of processor, if any.
              (with-error-policy (processor) (call-next-method)))

            (defclass ,restart-class-name ()
              ()
              (:documentation
               ,(format nil "This mixin class is intended to be mixed ~
                             into processor class that want to ~
                             establish a `continue' restart around the ~
                             execution of the `~(~A~)' method"
                        name)))

            (defmethod ,method :around ((processor ,restart-class-name)
                                        (data      t))
              ;; Establish `continue' restart around call to the next `handle'
              ;; method.
              (restart-case
                  (call-next-method)
                (continue (&optional condition)
                  :report (lambda (stream)
                            (format stream "~@<Ignore the failure to ~
                                            ~S datum ~A in ~A.~@:>"
                                    ',method data processor))
                  (declare (ignore condition)))))))))

  (define-error-policy+restart-mixins dispatch dispatcher)
  (define-error-policy+restart-mixins handle   handler))

;;; Mixin class `filtering-processor-mixin'

(defclass filtering-processor-mixin ()
  ((filter :initarg  :filter
           :type     (or null rsb.filter:conjoin-filter)
           :accessor processor-%filter
           :initform nil
           :documentation
           "When not null, the filter object is used to hold a list of
            filters that act in a conjunctive manner."))
  (:documentation
   "Adds filtering of events before further processing.

    Processors of this kind maintain a list of filters that can be
    accessed and modified using the `processor-filters'
    accessor. Events are dropped in a method on `handle' before any
    further processing unless they match all filters."))

(defmethod processor-filters ((processor filtering-processor-mixin))
  (when-let ((filter (processor-%filter processor)))
    (rsb.filter:filter-children filter)))

(defmethod (setf processor-filters) ((new-value null)
                                     (processor filtering-processor-mixin))
  (setf (processor-%filter processor) nil))

(defmethod (setf processor-filters) ((new-value list)
                                     (processor filtering-processor-mixin))
  (if-let ((filter (processor-%filter processor)))
    (setf (rsb.filter:filter-children filter) new-value)
    (setf (processor-%filter processor)
          (make-instance 'rsb.filter:conjoin-filter :children new-value)))
  new-value)

(defmethod handle :around ((processor filtering-processor-mixin)
                           (event     event))
  ;; Terminate processing of EVENT unless it matches PROCESSOR's
  ;; filters.
  (if-let ((filter (processor-%filter processor)))
    (when (rsb.filter:matches? filter event)
      (call-next-method))
    (call-next-method)))

;;; `deliver-timestamp-mixin' class

(defclass deliver-timestamp-mixin ()
  ()
  (:documentation
   "Can be mixed into event processor classes that used in event
    delivery contexts to attach :deliver timmestamps to processed
    events."))

(defmethod handle :before ((processor deliver-timestamp-mixin)
                           (event     event))
  ;; Attach a :deliver timestamp to EVENT.
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
   "Adds the ability to apply a transform to all handled data."))

(defmethod handle :around ((sink transform-mixin) (data t))
  (call-next-method sink (rsb.transform:transform! (processor-transform sink) data)))

;;; `sink-dispatcher-mixin'

(defclass sink-dispatcher-mixin ()
  ((sinks :accessor processor-%sinks
          :initform (make-sink-scope-trie)
          :documentation
          "Maps scopes to subscribed sinks."))
  (:documentation
   "Intended to be mixed into classes which dispatch based on a scope tree."))

(defmethod notify ((recipient sink-dispatcher-mixin)
                   (subject   t)
                   (action    subscribed))
  (let ((sinks (processor-%sinks recipient))
        (scope (subscription-scope action)))
    (sink-scope-trie-add sinks scope subject)
    (log:debug "~@<Scope trie of ~A after adding ~
                ~A:~@:_~/rsb.ep::print-trie/~@:>"
               recipient subject sinks)))

(defmethod notify ((recipient sink-dispatcher-mixin)
                   (subject   t)
                   (action    unsubscribed))
  (let ((sinks (processor-%sinks recipient))
        (scope (subscription-scope action)))
    (sink-scope-trie-remove sinks scope subject)
    (log:debug "~@<Scope trie of ~A after removing ~
                ~A:~@:_~/rsb.ep::print-trie/~@:>"
               recipient subject sinks)))

(defmethod dispatch ((processor sink-dispatcher-mixin)
                     (event     scope-and-event))
  (let+ ((scope (scope-and-event-scope event))
         ((&flet do-scope (components sinks)
            (declare (ignore components))
            (handle sinks (scope-and-event-event event)))))
    (declare (dynamic-extent #'do-scope))
    (scope-trie-map #'do-scope scope (processor-%sinks processor))))
