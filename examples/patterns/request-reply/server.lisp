;;;; server.lisp --- An example program demonstrating the local server.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Loading this file does not terminate.

;; mark-start::body
;; For managing the lifetime of `local-server' instances (e.g. for
;; short-lived clients), the `with-participant' macro can used. It
;; will take care of disposing of the `local-server' instance after it
;; has been used, also in case of non-local exist.
;;
;; Methods can be managed similarly. After the `with-methods' form,
;; the methods are removed.
;; mark-start::with-participant
(rsb:with-participant (server :local-server "/example/clientserver")
  (rsb.patterns.request-reply:with-methods (server)
      (("echo" (arg string)
         arg))))
;; mark-end::with-participant

;; mark-start::setf-method
(rsb:with-participant (server :local-server "/example/clientserver")
  (setf (rsb.patterns.request-reply:server-method server "echo")
        (lambda (arg) arg)))
;; mark-end::setf-method

;; Create a `local-server' instance that offers its methods under the
;; scope "/example/clientserver".
;; The local server will use all transports which are enabled in the
;; global RSB configuration with their respective configured options.
;;
;; The new server instance initially does not have any methods. There
;; are several ways to add methods.
;;
;; mark-start::variable
(defvar *local-server* (rsb:make-participant :local-server
                                             "/example/clientserver"))

(setf (rsb.patterns.request-reply:server-method *local-server* "echo")
      (lambda (arg) arg))

;; The local server and its methods will remain connected to the bus
;; until they are garbage collected or explicitly detached using the
;; `rsb:detach' function.

(rsb:detach *local-server*)
;; mark-end::variable

;; mark-end::body
