;;;; macros.lisp --- Convenience macros provided by the patterns.request-reply module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.patterns.request-reply)

(define-with-participant-macro remote-server)
(define-with-participant-macro local-server)

(defun call-with-methods (server methods thunk)
  (declare (type function thunk))
  (let+ (((&flet+ add-one-method ((name request-type lambda))
            (case request-type
              (:event (setf (server-method server name :argument :event)
                            lambda))
              (t      (setf (server-method server name)
                            lambda)))))
         ((&flet+ remove-one-method ((name &ign &ign))
            (when-let ((method (server-method server name :error? nil)))
              (handler-bind
                  (((or error bt:timeout)
                     (lambda (condition)
                       (warn "~@<Error removing method ~S: ~A~@:>"
                             method condition)
                       (continue))))
                (%remove-method-with-restart-and-timeout
                 server method))))))
    (unwind-protect
         (progn
           (mapc #'add-one-method methods)
           (funcall thunk))
      (mapc #'remove-one-method methods))))

(defmacro with-methods ((server) methods &body body)
  "Execute body with the methods defined by METHODS added to
SERVER. METHODS is a list of items of the form

  \(NAME ([ARG REQUEST-TYPE]) BODY)

where NAME is the name of the method, ARG is a symbol which will be
bound to the request data during the execution of the method body
BODY.

REQUEST-TYPE specifies the type of acceptable requests. If
REQUEST-TYPE is the keyword :event, BODY is called with ARG bound to
the request event (instead of just the payload).

If ARG and REQUEST-TYPE are omitted, the method does not accept
arguments and consequently BODY cannot access any argument binding
variable."
  (let+ (((&flet+ process-one ((name (&optional arg (request-type t))
                                &rest body))
            (let+ ((name/thunk  (symbolicate name '#:-method-body))
                   (name/string (when name (string name)))
                   ((&with-gensyms arg-var))
                   ((&values body declarations) (parse-body body)))
              (when name/string
                (check-type name/string method-name "a valid method name"))
              (check-type arg          symbol           "a symbol")
              (check-type request-type (or symbol cons) ":EVENT or a type specifier")

              `(list ,name/string ',request-type
                     (named-lambda ,name/thunk (,@(when arg `(,arg-var)))
                       ,@(when (and arg (not (member request-type '(:event t))))
                           `((check-type ,arg-var ,request-type)))
                       (let (,@(when arg `((,arg ,arg-var))))
                         ,@declarations
                         ,@body)))))))
    `(flet ((with-methods-thunk () ,@body))
       (declare (dynamic-extent #'with-methods-thunk))
       (call-with-methods ,server (list ,@(mapcar #'process-one methods))
                          #'with-methods-thunk))))
