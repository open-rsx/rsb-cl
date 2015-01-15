;;;; marcos.lisp --- Convenience marcos for RSB-related functionality.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

;;; Participants

(defun call-with-active-participant (participant thunk)
  "Call THUNK with PARTICIPANT as the sole argument.

   Deactivate PARTICIPANT when THUNK returns or a control transfer
   occurs."
  (declare (type function thunk))
  (unwind-protect
       (funcall thunk participant)
    (detach/ignore-errors participant)))

(defmacro with-active-participant ((var participant-form) &body body)
  "Execute BODY with VAR bound to the result of evaluating PARTICIPANT-FORM.

   Deactivate the participant when the execution of BODY ends normally
   or because of a control transfer."
  (check-type var symbol)
  `(flet ((with-participant-thunk (,var) ,@body))
     (declare (dynamic-extent #'with-participant-thunk))
     (call-with-active-participant ,participant-form #'with-participant-thunk)))

(defmacro with-active-participants (bindings &body body)
  "Execute BODY with participant bindings according to BINDINGS.

   Elements of BINDINGS are of the form

     (VAR PARTICIPANT-FORM)

   Deactivate all already constructed participants when constructing a
   participant causes a control transfer or when the execution of BODY
   ends normally or because of a control transfer."
  (destructuring-bind (&optional first &rest rest) bindings
    (if first
        `(with-active-participant ,first
           (with-active-participants ,rest ,@body))
        `(progn ,@body))))

(defmacro with-participant ((var kind scope
                             &rest initargs &key &allow-other-keys)
                            &body body)
  "Execute BODY with VAR bound to the participant KIND, SCOPE, INITARGS.

   Deactivate the participant when the execution of BODY ends normally
   or because of a control transfer."
  (check-type var symbol)
  `(with-active-participant (,var (make-participant ,kind ,scope ,@initargs))
     ,@body))

(defmacro with-participants (bindings &body body)
  "Execute BODY with participant bindings according to BINDINGS.

   Elements of BINDINGS are of the form

     (VAR KIND SCOPE &rest INITARGS &key &allow-other-keys)

   Deactivate all already constructed participants when constructing a
   participant causes a control transfer or when the execution of BODY
   ends normally or because of a control transfer."
  (destructuring-bind (&optional first &rest rest) bindings
    (if first
        `(with-participant ,first
           (with-participants ,rest ,@body))
        `(progn ,@body))))

;;; Handlers

(defun call-with-handler (listener handler thunk)
  "Call THUNK with HANDLER temporarily added to the handlers of
   LISTENER."
  (declare (type function handler thunk))
  (unwind-protect
       (progn
         (push handler (rsb.ep:handlers listener))
         (funcall thunk))
    (removef (rsb.ep:handlers listener) handler
             :test #'eq :count 1)))

(defmacro with-handler (listener
                        ((event-var) &body handler-body)
                        &body body)
  "Execute BODY with a handler executing HANDLER-BODY added to the
   handlers of LISTENER. The handler is of the form

     (lambda (EVENT-VAR) HANDLER-BODY)"
  (check-type event-var symbol "a symbol")
  `(flet ((with-handler-handler (,event-var) ,@handler-body)
          (with-handler-thunk () ,@body))
     (declare (dynamic-extent #'with-handler-handler #'with-handler-thunk))
     (call-with-handler
      ,listener #'with-handler-handler #'with-handler-thunk)))
