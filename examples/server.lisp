;;;; server.lisp --- An example program demonstrating the local server.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; mark-start::body
;; For managing the lifetime of `local-server' instances (e.g. for
;; short-lived clients), the `with-local-server' macro can used. It
;; will take care of disposing of the `remote-server' instance after
;; it has been used, also in case of non-local exist.
;;
;; Methods can be managed similarly. After the `with-methods' form,
;; the methods are removed.
;; mark-start::with-local-server
(rsb.patterns:with-local-server (server "/example/clientserver")
  (rsb.patterns:with-methods (server)
      (("echo" (arg string)
         arg))))
;; mark-end::with-local-server

;; mark-start::setf-method
(rsb.patterns:with-local-server (server "/example/clientserver")
  (setf (rsb.patterns:server-method server "echo")
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
;; The local server and its methods will remain connected to the bus
;; until they are garbage collected or explicitly detached using the
;; `detach' function.
;; mark-start::variable
(defvar *local-server* (rsb.patterns:make-local-server "/example/clientserver"))

(setf (rsb.patterns:server-method *local-server* "echo")
      (lambda (arg) arg))

(rsb:detach *local-server*)
;; mark-end::variable
;; mark-end::body
