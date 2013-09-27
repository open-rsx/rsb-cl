;;;; listener.lisp --- Listeners receive events that are broadcast on a bus.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb)

(defclass listener (receiving-client)
  ((handlers :initarg  :handlers
	     :type     list
	     :accessor rsb.ep:handlers
	     :initform nil
	     :documentation
	     "Stores the list of handlers two which events received by
this listener should be dispatched."))
  (:documentation
   "A listener consists of a set of filters, a set of handlers and has
a mechanism for dispatching matching events to these handlers."))

(defmethod (setf rsb.ep:handlers) :around ((new-value list)
					   (listener  listener))
  (let* ((configurator (rsb.ep:client-configurator listener))
	 (old-value    (rsb.ep:handlers listener))
	 (added        (set-difference new-value old-value))
	 (removed      (set-difference old-value new-value)))
    (prog1
	(call-next-method)
      (log1 :info listener "Added   handlers ~{~S~^, ~}" added)
      (log1 :info listener "Removed handlers ~{~S~^, ~}" removed)
      (iter (for handler in added)
	    (rsb.ep:notify configurator handler :handler-added))
      (iter (for handler in removed)
	    (rsb.ep:notify configurator handler :handler-removed)))))


;;; `listener' creation
;;

(defmethod make-listener ((scope scope)
			  &key
			  (transports (transport-options))
			  (converters (default-converters))
			  transform)
  ;; Translate different kinds of errors into
  ;; `listener-creation-failed' errors.
  (with-condition-translation
      (((error listener-creation-failed)
	:scope      scope
	:transports transports))
    (make-participant 'listener scope :in-push
		      transports converters transform)))

(define-participant-creation-uri-methods listener (scope puri:uri))

(define-participant-creation-restart-method listener (scope scope))
(define-participant-creation-restart-method listener (scope puri:uri))
