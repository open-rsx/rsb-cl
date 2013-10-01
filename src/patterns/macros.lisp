;;;; macros.lisp --- Convenience macros provided by the patterns module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.patterns)

(define-with-participant-macro remote-server)
(define-with-participant-macro local-server)

(defmacro with-methods ((var) methods &body body)
  "Execute body with the methods defined by METHODS added to the
server that is the value of VAR. METHODS is a list of items of the
form

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
  (check-type var symbol "a symbol")

  (let+ (((&flet+ process-one ((name (&optional arg (request-type t))
                                &rest body))
            (let+ ((name/string (string name))
                   ((&values body declarations) (parse-body body))
                   ((&with-gensyms arg-var)))
              (check-type name/string  method-name "a valid method name")
              (check-type arg          symbol      "a symbol")
              (check-type request-type symbol      ":EVENT or a symbol naming a type")

              (list
               ;; Create method lambda and add to server.
               `(setf (server-method ,var ,name/string
                                     ,@(when (eq request-type :event)
                                         `(:argument :event)))
                      #'(lambda (,@(when arg `(,arg-var)))
                          (let (,@(when arg `((,arg ,arg-var))))
                           ,@declarations
                           ,@(when (and arg (not (eq request-type :event)))
                                   `((check-type ,arg-var ,request-type)))
                           ,@body)))
               ;; Remove from server.
               `(when-let ((method (server-method ,var ,name/string :error? nil)))
                  (handler-bind
                      (((or error bt:timeout)
                        #'(lambda (condition)
                            (warn "~@<Error removing method ~S: ~A~@:>"
                                  method condition)
                            (continue))))
                    (%remove-method-with-restart-and-timeout
                     ,var method)))))))
         (add-and-remove (mapcar #'process-one methods)))
    `(unwind-protect
          (progn
            ,@(mapcar #'first add-and-remove)
            ,@body)
       ,@(mapcar #'second add-and-remove))))
