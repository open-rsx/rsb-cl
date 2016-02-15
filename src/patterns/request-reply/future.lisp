;;;; future.lisp --- A simple implementation of the future pattern.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

;;; Representation of failures

(defstruct (future-failure
            (:constructor make-future-failure (tag condition))
            (:copier      nil))
  (tag       nil :type keyword :read-only t)
  (condition nil :type list    :read-only t))

;;; `future' class

(defclass future ()
  ((result    :initarg  :result
              :writer   (setf future-%result)
              :documentation
              "Stores the result of the operation associated to the
               future. Remains unbound until the operation completes
               successfully or fails.")
   (lock      :reader   future-%lock
              :initform (bt:make-lock "Future lock")
              :documentation
              "Stores the lock that protects access to the result
               slot.")
   (condition :reader   future-%condition
              :initform (bt:make-condition-variable
                         :name "Future condition")
              :documentation
              "Stores the condition variable which can be used to wait
               for the result slot to be set."))
  (:documentation
   "Instances of this class represent results of operations that are
    still in progress when the respective instances are
    made. Instances can therefore be considered placeholders for the
    actual results which may or may not (when the producing operation
    fails) become available later.

    Interaction with `future' instances is done using methods on the
    following protocol functions:

    + `future-done?' :: Check whether the associated operation
      finished or failed.

    + `future-result' :: Obtained the result, potentially waiting for
      it to become available.

    + `(setf future-result)' :: Supply a result for the `future'
      instance.

    + `(setf future-error)' :: Indicate that the operation associated
      to the `future' instance failed.

    It is possible to supply values for the result, lock and condition
    slots of new `future' instance using initargs. The former may be
    useful when a result is immediately available but the future
    protocol has to be obeyed. The latter two may be useful when the
    lock and condition objects have to be available to some code
    outside the future or for performance reasons."))

(defmethod future-done? ((future future))
  (bt:with-lock-held ((future-%lock future))
    (when (slot-boundp future 'result)
      (let ((value (slot-value future 'result)))
        ;; When there is some result, check whether it indicates an
        ;; error.
        (if (future-failure-p value)
            (future-failure-tag value)
            :done)))))

(defmethod future-result ((future future)
                          &key
                          timeout
                          (error? t)
                          &allow-other-keys)
  (check-type timeout (or null timeout))

  (let+ (((&structure-r/o future- (lock %lock) (condition %condition))
          future)
         ((&flet timeout ()
            (if error?
                #+sbcl (error 'sb-ext:timeout :seconds timeout)
                ; #-sbcl #-sbcl #.(error "not implemented")
                (return-from future-result (values nil :timeout)))))
         (value (progn
                  (bt:with-lock-held (lock)
                    (iter (until (slot-boundp future 'result))
                          (unless (bt:condition-wait
                                   condition lock :timeout timeout)
                            (timeout))))
                  (slot-value future 'result))))
    #+later (declare (dynamic-extent #'timeout))
    ;; Return the result stored in VALUE or signal an error, depending
    ;; on ERROR?
    (cond
      ((not (future-failure-p value))
       (values value :done))

      ;; When the stored value indicates an error, signal the error or
      ;; return the tag, depending on ERROR?.
      (error?
       (apply #'error (future-failure-condition value)))

      (t
       (values nil (future-failure-tag value))))))

(defmethod (setf future-result) ((new-value t) (future future))
  (bt:with-lock-held ((future-%lock future))
    (prog1
        (setf (future-%result future) new-value)
      (bt:condition-notify (future-%condition future)))))

(defmethod (setf future-error) ((new-value t) (future future))
  ;; Store a result value that indicates an error. NEW-VALUE can be
  ;; used to (construct and) signal the error.
  (setf (future-result future)
        (make-future-failure :failed (ensure-list new-value))))

(defmethod print-object ((object future) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (or (future-done? object) :running))))
