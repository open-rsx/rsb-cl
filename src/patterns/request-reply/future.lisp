;;;; future.lisp --- A simple implementation of the future pattern.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; Representation of errors

(defconstant +future-failure-marker+
  (if (boundp '+future-failure-marker+)
      (symbol-value '+future-failure-marker+)
      (gensym "FUTURE-FAILURE-MARKER"))
  "This unique object is used to mark failure values as such and
distinguish them from regular results.")

(deftype future-failure-value ()
  "The extension of this type consists of all failure values."
  `(cons (eql ,+future-failure-marker+)
         (cons keyword
               (cons t null))))

(declaim (inline future-failure-tag future-failure-condition))

(defun future-failure-tag (value)
  "Return the tag of a failure value."
  (second value))

(defun future-failure-condition (value)
  "Return the condition data of a failure value."
  (third value))

;;; `future' class

(defclass future ()
  ((result    :initarg  :result
              :writer   (setf %future-result)
              :documentation
              "Stores the result of the operation associated to the
future. Remains unbound until the operation completes successfully or
fails.")
   (lock      :reader   %future-lock
              :initform (bt:make-lock "Future lock")
              :documentation
              "Stores the lock that protects access to the result
slot.")
   (condition :reader   %future-condition
              :initform (bt:make-condition-variable
                         :name "Future condition")
              :documentation
              "Stores the condition variable which can be used to wait
for the result slot to be set."))
  (:documentation
   "Instances of this class represent results of operations that are
still in progress when the respective instances are made. Instances
can therefore be considered placeholders for the actual results which
may or may not (when the producing operation fails) become available
later.

Interaction with `future' instances is done using methods on the
following protocol functions:
+ `future-done?' :: Check whether the associated operation finished or
    failed.
+ `future-result' :: Obtained the result, potentially waiting for it
    to become available.
+ `(setf future-result)' :: Supply a result for the `future' instance.
+ `(setf future-error)' :: Indicate that the operation associated to
    the `future' instance failed.

It is possible to supply values for the result, lock and condition
slots of new `future' instance using initargs. The former may be
useful when a result is immediately available but the future protocol
has to be obeyed. The latter two may be useful when the lock and
condition objects have to be available to some code outside the future
or for performance reasons."))

(defmethod future-done? ((future future))
  (bt:with-lock-held ((%future-lock future))
    (when (slot-boundp future 'result)
      (let ((value (slot-value future 'result)))
        ;; When there is some result, check whether it indicates an
        ;; error.
        (if (typep value 'future-failure-value)
            (future-failure-tag value)
            :done)))))

(defmethod future-result :around ((future future)
                                  &key
                                  timeout
                                  (error? t))
  (check-type timeout (or null timeout))

  (cond
    ;; If TIMEOUT has not been supplied, avoid the overhead and just
    ;; call the next method.
    ((not timeout)
     (call-next-method))

    ;; If TIMEOUT has been supplied and errors should be signaled,
    ;; only wait for the specified amount of time and let the timeout
    ;; condition through, should it be signaled.
    (error?
     (bt:with-timeout (timeout)
       (call-next-method)))

    ;; If TIMEOUT has been supplied, but error signaling is not
    ;; desired, handle timeout conditions turning them into result
    ;; values.
    (t
     (handler-case
         (bt:with-timeout (timeout)
           (call-next-method))
       (bt:timeout (condition)
         (declare (ignore condition))
         (values nil :timeout))))))

(defmethod future-result ((future future)
                          &key
                          (error? t)
                          &allow-other-keys)
  (let+ (((&accessors-r/o (lock      %future-lock)
                          (condition %future-condition)) future)
         (value (progn
                  (bt:with-lock-held (lock)
                    (iter (until (slot-boundp future 'result))
                          (bt:condition-wait condition lock)))
                  (slot-value future 'result))))
    ;; Return the result stored in VALUE or signal an error, depending
    ;; on ERROR?
    (cond
      ((not (typep value 'future-failure-value))
       (values value :done))

      ;; When the stored value indicates an error, signal the error or
      ;; return the tag, depending on ERROR?.
      (error?
       (apply #'error (future-failure-condition value)))

      (t
       (values nil (future-failure-tag value))))))

(defmethod (setf future-result) ((new-value t) (future future))
  (bt:with-lock-held ((%future-lock future))
    (prog1
        (setf (%future-result future) new-value)
      (bt:condition-notify (%future-condition future)))))

(defmethod (setf future-error) ((new-value t) (future future))
  ;; Store a result value that indicates an error. NEW-VALUE can be
  ;; used to (construct and) signal the error.
  (setf (future-result future) (%make-failure-value
                                (ensure-list new-value))))

(defmethod print-object ((object future) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (or (future-done? object) :running))))

;;; Utility functions

(defun %make-failure-value (condition-data)
  "Return an object that can be used to indicate a failed operation
for a condition described by CONDITION-DATA."
  (list +future-failure-marker+ :failed condition-data))
