;;;; sometimes-interruptible-mixin.lisp --- A threaded receiver that is sometimes interruptible.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.transport)

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
