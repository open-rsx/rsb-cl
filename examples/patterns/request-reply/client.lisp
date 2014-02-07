;;;; client.lisp --- An example program demonstrating the remote- server.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; mark-start::body
;; For managing the lifetime of `remote-server' instances (e.g. for
;; short-lived clients), the `with-remote-server' macro can used. It
;; will take care of disposing of the `remote-server' instance after
;; it has been used, also in case of non-local exist.
;; mark-start::with-remote-server
(rsb.patterns.request-reply:with-remote-server
    (remote-server "/example/clientserver")
  (format t "Server replied: ~A~%"
          (rsb.patterns.request-reply:call remote-server "echo" "bla")))
;; mark-end::with-remote-server

;; mark-start::calls
(rsb.patterns.request-reply:with-remote-server
    (remote-server "/example/clientserver")

  ;; The default behavior of returning the reply payload can be
  ;; changed using the :return keyword parameter.
  (rsb.patterns.request-reply:call remote-server "echo" "bla"
                                   :return :event)

  ;; Non-blocking calls can be made using the :block? keyword
  ;; parameter. In that case, an object implementing the future
  ;; protocol is returned to represent the result of the computation.
  (let ((future (rsb.patterns.request-reply:call remote-server "echo" "bla"
                                                 :block? nil)))
    (rsb.patterns.request-reply:future-result future))

  ;; These behaviors can be combined:
  (let ((future (rsb.patterns.request-reply:call remote-server "echo" "bla"
                                                 :block? nil
                                                 :return :event)))
    (rsb.patterns.request-reply:future-result future))

  ;; Another way of calling methods makes use of the fact that
  ;; `remote-method' instances are funcallable:
  (map 'list (rsb.patterns.request-reply:server-method remote-server "echo")
       '("a" "b" "c"))

  ;; This variant provides all the different behaviors of the `call'
  ;; variant:
  (funcall (rsb.patterns.request-reply:server-method remote-server "echo")
           "bla"
           :return :event
           :block? nil))
;; mark-end::calls

;; Create a `remote-server' instance that calls methods of the remote
;; server at "/example/clientserver".
;; The remote server will use all transports which are enabled in the
;; global RSB configuration with their respective configured options.
;;
;; Methods can be called without further preparation. Note that the
;; initial call of a method may take more time than subsequent methods
;; due to lazy initialization strategies.
;;
;; The remote server will remain connected to the bus until it is
;; garbage collected or explicitly detached using the `detach'
;; function.
;; mark-start::variable
(defvar *remote-server* (rsb.patterns.request-reply:make-remote-server
                         "/example/clientserver"))

(rsb.patterns.request-reply:call *remote-server* "echo" "bla")

(rsb:detach *remote-server*)
;; mark-end::variable
;; mark-end::body
