;;;; informer.lisp --- An example program demonstrating the listener.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; mark-start::body
;; For managing the lifetime of listeners (e.g. for short-lived
;; listeners), the `with-listener' macro can used. It will take care
;; of disposing of the `listener' instance after it has been used,
;; also in case of non-local exist.
;;
;; To register a handler with limited lifetime, the `with-handler'
;; macro can be used.
;; mark-start::with-listener
(rsb:with-listener (listener "/example/informer")
  (rsb::with-handler listener
      ((event)
       (format t "Received event: ~A~%" event))
    (sleep 20)))
;; mark-end::with-listener

;; This will create a `listener' instance that receives events which
;; are sent to the channel designated by the scope
;; "/example/listener". The listener will use all transports which are
;; enabled in the configuration with their respective configured
;; options.
;;
;; Just after creation, the listener will not act upon received
;; events. In order to process received events, handlers have to be
;; added to the listener. A handler is a function of one argument, the
;; event.
;;
;; The listener will participate in the channel until it is garbage
;; collected or explicitly detached from he channel.
;; mark-start::variable
(defvar *listener* (rsb:make-listener "/example/informer"))

(push (lambda (event)
        (format t "Received event: ~A~%" event))
      (rsb.ep:handlers *listener*))
;; mark-end::variable

;; In order to be notified about event receiving errors, additional
;; error handlers have to be registered.
(push (lambda (condition)
        (format t "Error: ~A~%" condition))
      (hooks:hook-handlers (hooks:object-hook *listener* 'rsb:error-hook)))
;; mark-end::body
