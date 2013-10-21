;;;; marcos.lisp --- Convenience marcos for RSB-related functionality.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb)

(defmacro define-with-participant-macro (kind &rest extra-args)
  (let ((name      (symbolicate "WITH-" kind))
        (make-name (symbolicate "MAKE-" kind)))
    `(defmacro ,name ((var scope-or-uri ,@extra-args
                       &rest args
                       &key
                       transports
                       converters
                       transform
                       &allow-other-keys)
                      &body body)

       ,(format nil "Execute BODY with VAR bound to a `~(~A~)' for the ~
                     channel designated by SCOPE-OR-URI. The ~:*~(~A~) ~
                     is destroyed when the execution of BODY ends ~
                     normally or because of a control transfer."
                kind)
       (declare (ignore transports converters transform))
       (check-type var symbol "a symbol")

       `(let ((,`,var (,',make-name ,`,scope-or-uri
                                    ,,@(mapcar (lambda (arg) `,arg)
                                               extra-args)
                                    ,@`,args)))
          (unwind-protect
               (progn ,@`,body)
            (detach/ignore-errors ,`,var))))))

(define-with-participant-macro listener)
(define-with-participant-macro reader)
(define-with-participant-macro informer type)

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
